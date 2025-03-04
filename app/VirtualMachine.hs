module VirtualMachine where

import Chunk (Chunk)
import Chunk qualified
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, asks, runReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify, put)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MVec
import Data.Word (Word8)
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

-- TODO: check for stack overflow
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

modifyInstruction :: (MonadState VMState m) => Int -> m ()
modifyInstruction i = modify (\vm -> vm{currentInstruction = i})

modifyGlobals :: (MonadState VMState m) => (GlobalScope -> GlobalScope) -> m ()
modifyGlobals f = modify (\vm -> vm{globals = f (globals vm)})

readByte :: (MonadReader Chunk m, MonadState VMState m) => m Word8
readByte = do
  idx <- gets currentInstruction
  modifyInstruction (idx + 1) *> asks (Chunk.readWord idx)

readConstant :: (MonadReader Chunk m, MonadState VMState m) => m Value
readConstant = readByte >>= (asks . Chunk.getValue) . fromIntegral

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

interpret :: (MonadReader Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m ()
interpret = do
  when debug $ do
    s <- get
    liftIO (showIOVector (stack s) (stackTop s) >>= putStrLn)
    debugCurrentInstruction
  op <- Chunk.fromWord <$> readByte
  case op of
    Chunk.OpReturn -> pure ()
    Chunk.OpConstant -> readConstant >>= (<* interpret) . push
    Chunk.OpNegate -> unOp consumeNum TNum negate <* interpret
    Chunk.OpAdd -> numBinOp (+) <* interpret
    Chunk.OpSub -> numBinOp (-) <* interpret
    Chunk.OpMul -> numBinOp (*) <* interpret
    Chunk.OpDiv -> numBinOp (/) <* interpret
    Chunk.OpNot -> unOp consumeBool TBool not <* interpret
    Chunk.OpLt -> boolBinOp (<) <* interpret
    Chunk.OpLeq -> boolBinOp (<=) <* interpret
    Chunk.OpGt -> boolBinOp (>) <* interpret
    Chunk.OpGeq -> boolBinOp (>=) <* interpret
    -- TODO: short circuit
    Chunk.OpOr -> boolBinOp (||) <* interpret
    Chunk.OpAnd -> boolBinOp (&&) <* interpret
    Chunk.OpEq -> (eq >>= push . TBool) <* interpret
    Chunk.OpNeq -> (eq >>= push . TBool . not) <* interpret
    Chunk.OpPop -> pop *> interpret
    Chunk.OpBindGlobal -> do
      v <- pop
      ident <- getStringConstant
      ref <- liftIO $ newIORef v
      modifyGlobals (Map.insert ident ref)
      interpret
    Chunk.OpSetGlobal -> do
      v <- pop
      ref <- getGlobalRef =<< getStringConstant
      liftIO $ writeIORef ref v
      interpret
    Chunk.OpGetGlobal -> do
      ref <- getGlobalRef =<< getStringConstant
      liftIO (readIORef ref) >>= push
      interpret
 where
  -- Chunk

  eq = do
    b <- pop
    a <- pop
    pure (valEq a b)

debugCurrentInstruction :: (MonadReader Chunk m, MonadState VMState m, MonadIO m) => m ()
debugCurrentInstruction = do
  idx <- gets currentInstruction
  chunk <- ask
  liftIO $ Chunk.disassembleInstruction_ idx chunk

runProgram :: (MonadError LoxError m, MonadIO m) => Chunk -> m ()
runProgram chunk = liftIO initialState >>= evalStateT (runReaderT interpret chunk)

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
