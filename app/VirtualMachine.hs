{-# LANGUAGE RecordWildCards #-}

module VirtualMachine where

import Chunk (Chunk)
import Chunk qualified
import Control.Monad (replicateM, void, when)
import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ask, asks, runReaderT)
import Control.Monad.State (MonadState (..), gets, modify, put)
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
  }

data Sign = Positive | Negative

type Callable m = [Chunk.Value] -> m Chunk.Value

stackSize :: Int
stackSize = 1024

debug :: Bool
debug = False

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

initialState :: IO VMState
initialState = do
  globals <- initialGlobals
  stack <- MVec.replicate 1024 Chunk.Nil
  let stackTop = 0
      currentInstruction = 0
  pure VMState{..}

peek :: (MonadState VMState m, MonadIO m) => m Chunk.Value
peek = do
  s <- get
  liftIO $ MVec.read (stack s) (stackTop s - 1)

push :: (MonadState VMState m, MonadIO m) => Chunk.Value -> m ()
push value = do
  s <- get
  liftIO $ MVec.write (stack s) (stackTop s) value
  put s{stackTop = stackTop s + 1}

pop :: (MonadState VMState m, MonadIO m) => m Chunk.Value
pop = do
  s <- get
  put (s{stackTop = stackTop s - 1})
  liftIO $ MVec.read (stack s) (stackTop s - 1)

modifyInstruction :: (MonadState VMState m) => (Int -> Int) -> m ()
modifyInstruction f = modify (\vm -> vm{currentInstruction = f (currentInstruction vm)})

modifyGlobals :: (MonadState VMState m) => (GlobalScope -> GlobalScope) -> m ()
modifyGlobals f = modify (\vm -> vm{globals = f (globals vm)})

-- TODO: add error handling
resetVM :: (MonadState VMState m) => m ()
resetVM = modify (\vm -> vm{currentInstruction = 0, stackTop = 0})

safely :: (MonadReader Chunk m, MonadState VMState m) => (Int -> m a) -> m (Maybe a)
safely f = do
  idx <- gets currentInstruction
  len <- asks (BS.length . Chunk.code)
  if idx < len
    then
      Just <$> f idx
    else pure Nothing

readWord :: (MonadReader Chunk m, MonadState VMState m) => m (Maybe Word8)
readWord = safely (asks . Chunk.readWord) <* modifyInstruction (+ 1)

readWord16 :: (MonadReader Chunk m, MonadState VMState m) => m (Maybe Word16)
readWord16 = safely (asks . Chunk.readWord16) <* modifyInstruction (+ 2)

readConstant :: (MonadReader Chunk m, MonadState VMState m) => m Chunk.Value
readConstant = readWord >>= (asks . Chunk.getValue) . fromIntegral . fromJust

showIOVector :: (Show a) => MVec.IOVector a -> Int -> IO String
showIOVector v n = do
  elements <- traverse (fmap show . MVec.read v) [0 .. n - 1]
  pure $ "[" ++ intercalate ", " elements ++ "]"

binOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  m a ->
  (b -> Chunk.Value) ->
  (a -> a -> b) ->
  m ()
binOp consume wrap op = do
  b <- consume
  a <- consume
  push . wrap $ op a b

numBinOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  (Double -> Double -> Double) ->
  m ()
numBinOp = binOp consumeNum Chunk.Num

boolBinOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  m a ->
  (a -> a -> Bool) ->
  m ()
boolBinOp m = binOp m Chunk.Bool

unOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  m a ->
  (a -> Chunk.Value) ->
  (a -> a) ->
  m ()
unOp consume wrap op = consume >>= push . wrap . op

whenJust :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJust m f =
  m >>= \case
    Just a -> f a
    Nothing -> pure ()

writeToStack :: (MonadState VMState m, MonadIO m) => Int -> Chunk.Value -> m ()
writeToStack idx value = do
  v <- gets stack
  liftIO $ MVec.write v idx value

readFromStack :: (MonadState VMState m, MonadIO m) => Int -> m Chunk.Value
readFromStack idx = gets stack >>= liftIO . flip MVec.read idx

signMultiplier :: Sign -> Int
signMultiplier = \case
  Positive -> 1
  Negative -> -1

jumpIf :: (MonadReader Chunk m, MonadState VMState m) => Sign -> Bool -> m ()
jumpIf sign b = do
  jumpLength <- fromIntegral . fromJust <$> readWord16
  let jumpDir = signMultiplier sign * jumpLength
  when b $ modifyInstruction (+ jumpDir)

eq :: (MonadState VMState m, MonadIO m) => m Bool
eq = do
  b <- pop
  a <- pop
  pure (Chunk.valEq a b)

interpret :: (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m ()
interpret = do
  when debug $ do
    s <- get
    liftIO (showIOVector (stack s) (stackTop s) >>= putStrLn)
    debugCurrentInstruction
  whenJust readWord $
    \op -> do
      case Chunk.fromWord op of
        Chunk.OpReturn -> pure ()
        Chunk.OpConstant -> readConstant >>= push
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
        Chunk.OpEq -> eq >>= push . Chunk.Bool
        Chunk.OpNeq -> eq >>= push . Chunk.Bool . not
        Chunk.OpPop -> void pop
        Chunk.OpIncrStack -> modify (\vm -> vm{stackTop = stackTop vm + 1})
        Chunk.OpBindGlobal -> do
          v <- pop
          ident <- getStringConstant
          ref <- liftIO $ newIORef v
          modifyGlobals (Map.insert ident ref)
        Chunk.OpSetGlobal -> do
          v <- peek
          ref <- getGlobalRef =<< getStringConstant
          liftIO $ writeIORef ref v
        Chunk.OpGetGlobal -> do
          ref <- getGlobalRef =<< getStringConstant
          liftIO (readIORef ref) >>= push
        Chunk.OpSetLocal -> do
          v <- peek
          offset <- fromIntegral . fromJust <$> readWord
          writeToStack offset v
        Chunk.OpGetLocal -> do
          offset <- fromIntegral . fromJust <$> readWord
          readFromStack offset >>= push
        Chunk.OpJumpIfFalse -> peek >>= jumpIf Positive . not . truthyValue
        Chunk.OpJumpIfTrue -> peek >>= jumpIf Positive . truthyValue
        Chunk.OpJump -> jumpIf Positive True
        Chunk.OpLoop -> jumpIf Negative True
        Chunk.OpCall -> do
          nArgs <- fromIntegral . fromJust <$> readWord
          args <- reverse <$> replicateM nArgs pop
          f <- consumeCallable
          f args >>= push
      interpret

debugCurrentInstruction :: (MonadReader Chunk m, MonadState VMState m, MonadIO m) => m ()
debugCurrentInstruction = do
  idx <- gets currentInstruction
  chunk <- ask
  when (idx < BS.length (Chunk.code chunk)) $
    liftIO (Chunk.disassembleInstruction_ idx chunk)

runProgram :: (MonadError LoxError m, MonadIO m, MonadState VMState m) => Chunk -> m ()
runProgram = runReaderT interpret

throwExprError :: (MonadError LoxError m, MonadState VMState m, MonadReader Chunk m) => ExprError_ -> m a
throwExprError err = do
  idx <- gets currentInstruction
  chunk <- ask
  let l = Chunk.getSourcePos idx chunk
  throwError (exprError l err)

getGlobalRef :: (MonadState VMState m, MonadError LoxError m, MonadReader Chunk m) => String -> m (IORef Chunk.Value)
getGlobalRef ident = do
  curGlobals <- gets globals
  case Map.lookup ident curGlobals of
    Nothing -> throwExprError $ NotInScope ident
    Just ref -> pure ref

currentLocation :: (MonadReader Chunk m, MonadState VMState m) => m SourcePos
currentLocation = do
  idx <- gets currentInstruction
  asks (Chunk.getSourcePos idx)

reportError :: (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m) => ExprError_ -> m a
reportError e = do
  l <- currentLocation
  throwError . exprError l $ e

consumeNum :: (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m Double
consumeNum = do
  v <- pop
  case v of
    Chunk.Num x -> pure x
    _ -> reportError $ TypeError "num" (Text.unpack $ Chunk.valuePretty v)

consumeCallable :: (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m (Callable m)
consumeCallable =
  pop >>= \case
    Chunk.NativeFunction f -> do
      l <- currentLocation
      pure (\vs -> liftIO (f vs l) >>= liftEither)
    v -> reportError $ TypeError "callable" (Text.unpack $ Chunk.valuePretty v)

truthyValue :: Chunk.Value -> Bool
truthyValue = \case
  Chunk.Bool b -> b
  Chunk.Num x -> (x /= 0.0)
  Chunk.Nil -> False
  Chunk.String s -> (not $ null s)
  Chunk.NativeFunction _ -> True
  Chunk.Function{} -> True

consumeBool :: (MonadState VMState m, MonadIO m) => m Bool
consumeBool = fmap truthyValue pop

getStringConstant :: (MonadState VMState m, MonadReader Chunk m) => m String
getStringConstant =
  readConstant >>= \case
    Chunk.String ident -> pure ident
    _ -> undefined
