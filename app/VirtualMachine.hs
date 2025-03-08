module VirtualMachine where

import Chunk (Chunk)
import Chunk qualified
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, asks, runReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify, put)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MVec
import Data.Word (Word16, Word8)
import Types

type GlobalScope = Map String (IORef Value)

data VMState = VMState
  { -- in the book, this is represented as just a ByteString, but i'm not sure that's practical...
    globals :: GlobalScope
  , currentInstruction :: Int
  , stackTop :: Int
  , stack :: MVec.IOVector Value
  }

stackSize :: Int
stackSize = 1024

debug :: Bool
debug = True

initialState :: IO VMState
initialState = VMState Map.empty 0 0 <$> MVec.replicate 1024 TNil

peek :: (MonadState VMState m, MonadIO m) => m Value
peek = do
  s <- get
  liftIO $ MVec.read (stack s) (stackTop s - 1)

push :: (MonadState VMState m, MonadIO m) => Value -> m ()
push value = do
  s <- get
  liftIO $ MVec.write (stack s) (stackTop s) value
  put s{stackTop = stackTop s + 1}

pop :: (MonadState VMState m, MonadIO m) => m Value
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

readConstant :: (MonadReader Chunk m, MonadState VMState m) => m Value
readConstant = readWord >>= (asks . Chunk.getValue) . fromIntegral . fromJust

showIOVector :: (Show a) => MVec.IOVector a -> Int -> IO String
showIOVector v n = do
  elements <- traverse (fmap show . MVec.read v) [0 .. n - 1]
  pure $ "[" ++ intercalate ", " elements ++ "]"

binOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  m a ->
  (a -> Value) ->
  (a -> a -> a) ->
  m ()
binOp consume wrap op = do
  b <- consume
  a <- consume
  push . wrap $ op a b

numBinOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  (Double -> Double -> Double) ->
  m ()
numBinOp = binOp consumeNum TNum

boolBinOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  (Bool -> Bool -> Bool) ->
  m ()
boolBinOp = binOp consumeBool TBool

unOp ::
  (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) =>
  m a ->
  (a -> Value) ->
  (a -> a) ->
  m ()
unOp consume wrap op = consume >>= push . wrap . op

valEq :: Value -> Value -> Bool
valEq (TNum x) (TNum y) = x == y
valEq (TBool b1) (TBool b2) = b1 == b2
valEq (TString s1) (TString s2) = s1 == s2
valEq TNil TNil = True
valEq _ _ = False

whenJust :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJust m f =
  m >>= \case
    Just a -> f a
    Nothing -> pure ()

writeToStack :: (MonadState VMState m, MonadIO m) => Int -> Value -> m ()
writeToStack idx value = do
  v <- gets stack
  liftIO $ MVec.write v idx value

readFromStack :: (MonadState VMState m, MonadIO m) => Int -> m Value
readFromStack idx = gets stack >>= liftIO . flip MVec.read idx

jump :: (MonadReader Chunk m, MonadState VMState m) => m ()
jump = readWord16 >>= (\jl -> modifyInstruction (+ jl)) . fromIntegral . fromJust

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
        Chunk.OpNegate -> unOp consumeNum TNum negate
        Chunk.OpAdd -> numBinOp (+)
        Chunk.OpSub -> numBinOp (-)
        Chunk.OpMul -> numBinOp (*)
        Chunk.OpDiv -> numBinOp (/)
        Chunk.OpNot -> unOp consumeBool TBool not
        Chunk.OpLt -> boolBinOp (<)
        Chunk.OpLeq -> boolBinOp (<=)
        Chunk.OpGt -> boolBinOp (>)
        Chunk.OpGeq -> boolBinOp (>=)
        -- TODO: short circuit
        Chunk.OpOr -> boolBinOp (||)
        Chunk.OpAnd -> boolBinOp (&&)
        Chunk.OpEq -> eq >>= push . TBool
        Chunk.OpNeq -> eq >>= push . TBool . not
        Chunk.OpPop -> void pop
        Chunk.OpBindGlobal -> do
          v <- pop
          ident <- getStringConstant
          ref <- liftIO $ newIORef v
          modifyGlobals (Map.insert ident ref)
        Chunk.OpSetGlobal -> do
          v <- pop
          ref <- getGlobalRef =<< getStringConstant
          liftIO $ writeIORef ref v
        Chunk.OpGetGlobal -> do
          ref <- getGlobalRef =<< getStringConstant
          liftIO (readIORef ref) >>= push
        Chunk.OpSetLocal -> do
          v <- pop
          offset <- fromIntegral . fromJust <$> readWord
          writeToStack offset v
        Chunk.OpGetLocal -> do
          offset <- fromIntegral . fromJust <$> readWord
          readFromStack offset >>= push
        Chunk.OpJumpIfFalse -> do
          b <- truthyValue <$> peek
          if b then void readWord16 else jump
        Chunk.OpJump -> jump
      interpret
 where
  eq = do
    b <- pop
    a <- pop
    pure (valEq a b)

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

getGlobalRef :: (MonadState VMState m, MonadError LoxError m, MonadReader Chunk m) => String -> m (IORef Value)
getGlobalRef ident = do
  curGlobals <- gets globals
  case Map.lookup ident curGlobals of
    Nothing -> throwExprError $ NotInScope ident
    Just ref -> pure ref

consumeNum :: (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m Double
consumeNum = do
  v <- pop
  case v of
    TNum x -> pure x
    _ -> do
      idx <- gets currentInstruction
      chunk <- ask
      let l = Chunk.getSourcePos idx chunk
      throwError (exprError l $ TypeError "num" v)

truthyValue :: Value -> Bool
truthyValue = \case
  TBool b -> b
  TNum x -> (x /= 0.0)
  TNil -> False
  TString s -> (not $ null s)
  TNativeFunction _ -> True
  TFunction{} -> True

consumeBool :: (MonadState VMState m, MonadIO m) => m Bool
consumeBool = fmap truthyValue pop

getStringConstant :: (MonadState VMState m, MonadReader Chunk m) => m String
getStringConstant =
  readConstant >>= \case
    TString ident -> pure ident
    _ -> undefined
