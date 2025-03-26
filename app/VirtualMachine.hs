{-# LANGUAGE RecordWildCards #-}

module VirtualMachine where

import Bluefin.Eff (Eff, (:>))
import Bluefin.Exception (Exception, throw)
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State, get, modify, put)
import Chunk (Chunk)
import Chunk qualified
import Control.Monad (replicateM, void, when)
import Data.ByteString qualified as BS
import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector.Mutable qualified as MVec
import Data.Word (Word16, Word8)
import Error
import Types (SourcePos)

type GlobalScope = Map String (IORef Chunk.Value)

data CallFrame = CallFrame
  { func :: Chunk.Function
  , ip :: Int
  , stackOffset :: Int
  }

data VMState = VMState
  { -- in the book, this is represented as just a ByteString, but i'm not sure that's practical...
    globals :: GlobalScope
  , frames :: NonEmpty CallFrame
  , stackTop :: Int
  , stack :: MVec.IOVector Chunk.Value
  , debug :: Bool
  }

data Sign = Positive | Negative

type Callable es = [Chunk.Value] -> Eff es Chunk.Value

gets :: (e1 :> es) => State a e1 -> (a -> b) -> Eff es b
gets state f = fmap f (get state)

-- globals

clock :: Chunk.NativeFunction
clock [] _ = do
  Right . Chunk.Num . (fromIntegral @Int @Double) . round . (* 1000) <$> getPOSIXTime
clock args l = pure (Left $ exprError l $ Arity "clock" 0 (length args))

printy :: Chunk.NativeFunction
printy [] _ = putStrLn "" $> Right Chunk.Nil
printy (x : xs) l = TIO.putStr (Chunk.valuePretty x <> " ") *> printy xs l

initialGlobals :: IO GlobalScope
initialGlobals = traverse newIORef $ Map.fromList [("clock", Chunk.NativeFunction clock), ("print", Chunk.NativeFunction printy)]

initialState :: Bool -> IO VMState
initialState debug = do
  globals <- initialGlobals
  stack <- MVec.replicate 1024 Chunk.Nil
  let stackTop = 0
      -- this _should_ be ok because resetVM should be called before execution begins
      frames = undefined
  pure VMState{..}

peek :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Eff es Chunk.Value
peek state io = do
  s <- get state
  effIO io $ MVec.read (stack s) (stackTop s - 1)

push :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Chunk.Value -> Eff es ()
push state io value = do
  s <- get state
  effIO io $ MVec.write (stack s) (stackTop s) value
  put state s{stackTop = stackTop s + 1}

pop :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Eff es Chunk.Value
pop state io = do
  s <- get state
  put state (s{stackTop = stackTop s - 1})
  effIO io $ MVec.read (stack s) (stackTop s - 1)

-- effIO io $ MVec.write (stack s) (stackTop s - 1) Chunk.Nil
-- pure res

modifyInstruction :: (e1 :> es) => State VMState e1 -> (Int -> Int) -> Eff es ()
modifyInstruction state f =
  modify
    state
    -- maybe lenses would be good..
    ( \vm ->
        let frames' = frames vm
            fr = NonEmpty.head frames'
            frs = NonEmpty.tail frames'
         in vm{frames = fr{ip = f (ip fr)} :| frs}
    )

modifyGlobals :: (e1 :> es) => State VMState e1 -> (GlobalScope -> GlobalScope) -> Eff es ()
modifyGlobals state f = modify state (\vm -> vm{globals = f (globals vm)})

topLevelFrame :: Chunk.Function -> CallFrame
topLevelFrame func = CallFrame func 0 0

-- TODO: add error handling
resetVM :: (e1 :> es) => State VMState e1 -> Chunk.Function -> Eff es ()
resetVM state func = modify state (\vm -> vm{frames = (topLevelFrame func) :| [], stackTop = 0})

safely :: (e1 :> es) => State VMState e1 -> (Int -> Chunk -> a) -> Eff es (Maybe a)
safely state f = do
  frame <- gets state (NonEmpty.head . frames)
  let idx = ip frame
      chunk = (Chunk.chunk . func) frame
      len = (BS.length . Chunk.code) chunk
  pure $
    if idx < len
      then
        Just $ f idx chunk
      else Nothing

currentChunk :: (e1 :> es) => State VMState e1 -> Eff es Chunk
currentChunk state = gets state (Chunk.chunk . func . NonEmpty.head . frames)

readWord :: (e1 :> es) => State VMState e1 -> Eff es (Maybe Word8)
readWord state = (safely state Chunk.readWord) <* modifyInstruction state (+ 1)

readWord16 :: (e1 :> es) => State VMState e1 -> Eff es (Maybe Word16)
readWord16 state = (safely state Chunk.readWord16) <* modifyInstruction state (+ 2)

readConstant :: (e1 :> es) => State VMState e1 -> Eff es Chunk.Value
readConstant state = do
  idx <- fromIntegral . fromJust <$> readWord state
  Chunk.getValue idx <$> currentChunk state

showIOVector :: (Show a) => MVec.IOVector a -> Int -> IO String
showIOVector v n = do
  elements <- traverse (fmap show . MVec.read v) [0 .. n - 1]
  pure $ "[" ++ intercalate ", " elements ++ "]"

whenJust :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJust m f =
  m >>= \case
    Just a -> f a
    Nothing -> pure ()

writeToStack :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Int -> Chunk.Value -> Eff es ()
writeToStack state io idx value = do
  v <- gets state stack
  effIO io $ MVec.write v idx value

readFromStack :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Int -> Eff es Chunk.Value
readFromStack state io idx = gets state stack >>= effIO io . flip MVec.read idx

signMultiplier :: Sign -> Int
signMultiplier = \case
  Positive -> 1
  Negative -> -1

jumpIf :: (e1 :> es) => State VMState e1 -> Sign -> Bool -> Eff es ()
jumpIf state sign b = do
  jumpLength <- fromIntegral . fromJust <$> readWord16 state
  let jumpDir = signMultiplier sign * jumpLength
  when b $ modifyInstruction state (+ jumpDir)

eq :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Eff es Bool
eq state io = do
  b <- pop state io
  a <- pop state io
  pure (Chunk.valEq a b)

interpret ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  State VMState e1 ->
  Exception LoxError e2 ->
  IOE e3 ->
  Eff es ()
interpret state exn io = do
  s <- get state
  when (debug s) $ do
    effIO io (showIOVector (stack s) (stackTop s) >>= putStrLn)
    debugCurrentInstruction state io

  whenJust (readWord state) $
    \op -> do
      case Chunk.fromWord op of
        Chunk.OpReturn -> pure ()
        Chunk.OpConstant -> readConstant' >>= push'
        Chunk.OpNegate -> unOp consumeNum Chunk.Num negate
        Chunk.OpAdd -> numBinOp (+)
        Chunk.OpSub -> numBinOp (-)
        Chunk.OpMul -> numBinOp (*)
        Chunk.OpDiv -> numBinOp (/)
        Chunk.OpNot -> unOp consumeBool Chunk.Bool not
        Chunk.OpLt -> boolBinOp consumeNum (<)
        Chunk.OpLeq -> boolBinOp consumeNum (<=)
        Chunk.OpGt -> boolBinOp consumeNum (>)
        Chunk.OpGeq -> boolBinOp consumeNum (>=)
        -- TODO: short circuit
        Chunk.OpOr -> boolBinOp consumeBool (||)
        Chunk.OpAnd -> boolBinOp consumeBool (&&)
        Chunk.OpEq -> eq' >>= push' . Chunk.Bool
        Chunk.OpNeq -> eq' >>= push' . Chunk.Bool . not
        Chunk.OpPop -> void pop'
        Chunk.OpIncrStack -> modify state (\vm -> vm{stackTop = stackTop vm + 1})
        Chunk.OpBindGlobal -> do
          v <- pop'
          ident <- getStringConstant'
          ref <- effIO io $ newIORef v
          modifyGlobals state (Map.insert ident ref)
        Chunk.OpSetGlobal -> do
          v <- peek'
          ref <- getGlobalRef' =<< getStringConstant'
          effIO io $ writeIORef ref v
        Chunk.OpGetGlobal -> do
          ref <- getGlobalRef' =<< getStringConstant'
          effIO io (readIORef ref) >>= push'
        Chunk.OpSetLocal -> do
          v <- peek'
          offset <- fromIntegral . fromJust <$> readWord'
          writeToStack state io offset v
        Chunk.OpGetLocal -> do
          offset <- fromIntegral . fromJust <$> readWord'
          readFromStack state io offset >>= push'
        Chunk.OpJumpIfFalse -> peek' >>= jumpIf' Positive . not . truthyValue
        Chunk.OpJumpIfTrue -> peek' >>= jumpIf' Positive . truthyValue
        Chunk.OpJump -> jumpIf' Positive True
        Chunk.OpLoop -> jumpIf' Negative True
        Chunk.OpCall -> do
          nArgs <- fromIntegral . fromJust <$> readWord'
          args <- reverse <$> replicateM nArgs pop'
          f <- consumeCallable
          f args >>= push'
      interpret state exn io
 where
  -- surely a better way?
  binOp consume wrap op = do
    b <- consume
    a <- consume
    push state io . wrap $ op a b

  numBinOp = binOp consumeNum Chunk.Num
  boolBinOp e = binOp e Chunk.Bool
  unOp consume wrap op = consume >>= push state io . wrap . op

  consumeNum = do
    v <- pop state io
    case v of
      Chunk.Num x -> pure x
      _ -> throwExprError state exn $ TypeError "num" (Text.unpack $ Chunk.valuePretty v)
  consumeCallable =
    pop state io >>= \case
      Chunk.NativeFunction f -> do
        l <- currentLocation state
        pure (\vs -> effIO io (f vs l) >>= either (throw exn) pure)
      v ->
        throwExprError state exn $
          TypeError "callable" (Text.unpack $ Chunk.valuePretty v)
  consumeBool = fmap truthyValue (pop state io)

  readConstant' = readConstant state
  getStringConstant' = getStringConstant state
  getGlobalRef' = getGlobalRef state exn
  push' = push state io
  pop' = pop state io
  peek' = peek state io
  eq' = eq state io
  jumpIf' = jumpIf state
  readWord' = readWord state

debugCurrentInstruction ::
  (e1 :> es, e2 :> es) =>
  State VMState e1 ->
  IOE e2 ->
  Eff es ()
debugCurrentInstruction state io = do
  idx <- gets state (ip . NonEmpty.head . frames)
  chunk <- currentChunk state
  when (idx < BS.length (Chunk.code chunk)) $
    effIO io (Chunk.disassembleInstruction_ idx chunk)

runProgram :: (e1 :> es, e2 :> es, e3 :> es) => State VMState e1 -> Exception LoxError e2 -> IOE e3 -> Chunk.Function -> Eff es ()
runProgram state exn io func = resetVM state func *> interpret state exn io

throwExprError ::
  (e1 :> es, e2 :> es) =>
  State VMState e1 ->
  Exception LoxError e2 ->
  ExprError_ ->
  Eff es a
throwExprError state exn err = do
  idx <- gets state (ip . NonEmpty.head . frames)
  chunk <- currentChunk state
  let l = Chunk.getSourcePos idx chunk
  throw exn (exprError l err)

getGlobalRef ::
  (e1 :> es, e2 :> es) =>
  State VMState e1 ->
  Exception LoxError e2 ->
  String ->
  Eff es (IORef Chunk.Value)
getGlobalRef state exn ident = do
  curGlobals <- gets state globals
  case Map.lookup ident curGlobals of
    Nothing -> throwExprError state exn $ NotInScope ident
    Just ref -> pure ref

currentLocation :: (e1 :> es) => State VMState e1 -> Eff es SourcePos
currentLocation state = do
  idx <- gets state (ip . NonEmpty.head . frames)
  (Chunk.getSourcePos idx) <$> currentChunk state

truthyValue :: Chunk.Value -> Bool
truthyValue = \case
  Chunk.Bool b -> b
  Chunk.Num x -> (x /= 0.0)
  Chunk.Nil -> False
  Chunk.String s -> (not $ null s)
  Chunk.NativeFunction _ -> True
  Chunk.VFunction _ -> True

getStringConstant :: (e1 :> es) => State VMState e1 -> Eff es String
getStringConstant state =
  readConstant state >>= \case
    Chunk.String ident -> pure ident
    _ -> undefined
