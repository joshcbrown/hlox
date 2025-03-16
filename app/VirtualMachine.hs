{-# LANGUAGE RecordWildCards #-}

module VirtualMachine where

import Bluefin
import Bluefin.Eff (Eff, (:>))
import Bluefin.Exception (Exception, throw)
import Bluefin.IO (IOE, effIO)
import Bluefin.Reader
import Bluefin.State (State, evalState, get, modify, put)
import Chunk (Chunk)
import Chunk qualified
import Control.Monad (replicateM, void, when)
import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
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

data VMState = VMState
  { -- in the book, this is represented as just a ByteString, but i'm not sure that's practical...
    globals :: GlobalScope
  , currentInstruction :: Int
  , stackTop :: Int
  , stack :: MVec.IOVector Chunk.Value
  , debug :: Bool
  }

data Sign = Positive | Negative

type Callable es = [Chunk.Value] -> Eff es Chunk.Value

gets :: (e1 :> es) => State a e1 -> (a -> b) -> Eff es b
gets state f = fmap f (get state)

stackSize :: Int
stackSize = 1024

-- globals

clock :: Chunk.NativeFunction
clock [] _ = do
  Right . Chunk.Num . (fromIntegral @Int @Double) . round . (* 1000) <$> getPOSIXTime
clock args l = pure (Left $ exprError l $ Arity "clock" 0 (length args))

printy :: Chunk.NativeFunction
printy [] _ = putStrLn "" $> Right Chunk.Nil
printy (x : xs) l = TIO.putStr (Chunk.valuePretty x <> " ") *> printy xs l

insertIORefs :: (Traversable t) => t a -> IO (t (IORef a))
insertIORefs = traverse newIORef

initialGlobals :: IO GlobalScope
initialGlobals = insertIORefs $ Map.fromList [("clock", Chunk.NativeFunction clock), ("print", Chunk.NativeFunction printy)]

initialState :: Bool -> IO VMState
initialState debug = do
  globals <- initialGlobals
  stack <- MVec.replicate 1024 Chunk.Nil
  let stackTop = 0
      currentInstruction = 0
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

modifyInstruction :: (e1 :> es) => State VMState e1 -> (Int -> Int) -> Eff es ()
modifyInstruction state f = modify state (\vm -> vm{currentInstruction = f (currentInstruction vm)})

modifyGlobals :: (e1 :> es) => State VMState e1 -> (GlobalScope -> GlobalScope) -> Eff es ()
modifyGlobals state f = modify state (\vm -> vm{globals = f (globals vm)})

-- TODO: add error handling
resetVM :: (e1 :> es) => State VMState e1 -> Eff es ()
resetVM state = modify state (\vm -> vm{currentInstruction = 0, stackTop = 0})

safely :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> (Int -> Eff es a) -> Eff es (Maybe a)
safely reader state f = do
  idx <- gets state currentInstruction
  len <- asks reader (BS.length . Chunk.code)
  if idx < len
    then
      Just <$> f idx
    else pure Nothing

readWord :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> Eff es (Maybe Word8)
readWord reader state = (safely reader state (asks reader . Chunk.readWord)) <* modifyInstruction state (+ 1)

readWord16 :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> Eff es (Maybe Word16)
readWord16 reader state = (safely reader state (asks reader . Chunk.readWord16)) <* modifyInstruction state (+ 2)

readConstant :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> Eff es Chunk.Value
readConstant reader state = readWord reader state >>= (asks reader . Chunk.getValue) . fromIntegral . fromJust

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

jumpIf :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> Sign -> Bool -> Eff es ()
jumpIf reader state sign b = do
  jumpLength <- fromIntegral . fromJust <$> readWord16 reader state
  let jumpDir = signMultiplier sign * jumpLength
  when b $ modifyInstruction state (+ jumpDir)

eq :: (e1 :> es, e2 :> es) => State VMState e1 -> IOE e2 -> Eff es Bool
eq state io = do
  b <- pop state io
  a <- pop state io
  pure (Chunk.valEq a b)

interpret ::
  (e1 :> es, e2 :> es, e3 :> es, e4 :> es) =>
  Reader Chunk e1 ->
  State VMState e2 ->
  Exception LoxError e3 ->
  IOE e4 ->
  Eff es ()
interpret reader state exn io = do
  s <- get state
  when (debug s) $ do
    effIO io (showIOVector (stack s) (stackTop s) >>= putStrLn)
    debugCurrentInstruction reader state io

  whenJust (readWord reader state) $
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
      interpret reader state exn io
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
      _ -> throwExprError reader state exn $ TypeError "num" (Text.unpack $ Chunk.valuePretty v)
  consumeCallable =
    pop state io >>= \case
      Chunk.NativeFunction f -> do
        l <- currentLocation reader state
        pure (\vs -> effIO io (f vs l) >>= either (throw exn) pure)
      v ->
        throwExprError reader state exn $
          TypeError "callable" (Text.unpack $ Chunk.valuePretty v)
  consumeBool = fmap truthyValue (pop state io)

  readConstant' = readConstant reader state
  getStringConstant' = getStringConstant reader state
  getGlobalRef' = getGlobalRef reader state exn
  push' = push state io
  pop' = pop state io
  peek' = peek state io
  eq' = eq state io
  jumpIf' = jumpIf reader state
  readWord' = readWord reader state

debugCurrentInstruction ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Reader Chunk e1 ->
  State VMState e2 ->
  IOE e3 ->
  Eff es ()
debugCurrentInstruction reader state io = do
  idx <- gets state currentInstruction
  chunk <- ask reader
  when (idx < BS.length (Chunk.code chunk)) $
    effIO io (Chunk.disassembleInstruction_ idx chunk)

runProgram :: (e1 :> es, e2 :> es, e3 :> es) => State VMState e1 -> Exception LoxError e2 -> IOE e3 -> Chunk -> Eff es ()
runProgram state exn io chunk = runReader chunk (\reader -> interpret reader state exn io)

throwExprError ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Reader Chunk e1 ->
  State VMState e2 ->
  Exception LoxError e3 ->
  ExprError_ ->
  Eff es a
throwExprError reader state exn err = do
  idx <- gets state currentInstruction
  chunk <- ask reader
  let l = Chunk.getSourcePos idx chunk
  throw exn (exprError l err)

getGlobalRef ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Reader Chunk e1 ->
  State VMState e2 ->
  Exception LoxError e3 ->
  String ->
  Eff es (IORef Chunk.Value)
getGlobalRef reader state exn ident = do
  curGlobals <- gets state globals
  case Map.lookup ident curGlobals of
    Nothing -> throwExprError reader state exn $ NotInScope ident
    Just ref -> pure ref

currentLocation :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> Eff es SourcePos
currentLocation reader state = do
  idx <- gets state currentInstruction
  asks reader (Chunk.getSourcePos idx)

truthyValue :: Chunk.Value -> Bool
truthyValue = \case
  Chunk.Bool b -> b
  Chunk.Num x -> (x /= 0.0)
  Chunk.Nil -> False
  Chunk.String s -> (not $ null s)
  Chunk.NativeFunction _ -> True
  Chunk.Function{} -> True

getStringConstant :: (e1 :> es, e2 :> es) => Reader Chunk e1 -> State VMState e2 -> Eff es String
getStringConstant reader state =
  readConstant reader state >>= \case
    Chunk.String ident -> pure ident
    _ -> undefined
