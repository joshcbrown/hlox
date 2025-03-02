module VirtualMachine where

import Chunk qualified
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, asks, runReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify, put)
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MVec
import Data.Word (Word8)
import Types

data VMState = VMState
  { -- in the book, this is represented as just a ByteString, but i'm not sure that's practical...
    currentInstruction :: Int
  , stackTop :: Int
  , stack :: MVec.IOVector Chunk.Value
  }

stackSize :: Int
stackSize = 1024

debug :: Bool
debug = True

initialState :: IO VMState
initialState = VMState 0 0 <$> MVec.replicate 1024 0

-- TODO: check for stack overflow
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

modifyInstruction :: (MonadState VMState m) => Int -> m ()
modifyInstruction i = modify (\vm -> vm{currentInstruction = i})

readByte :: (MonadReader Chunk.Chunk m, MonadState VMState m) => m Word8
readByte = do
  idx <- gets currentInstruction
  modifyInstruction (idx + 1) *> asks (Chunk.readWord idx)

readConstant :: (MonadReader Chunk.Chunk m, MonadState VMState m) => m Chunk.Value
readConstant = readByte >>= (asks . Chunk.getValue) . fromIntegral

showIOVector :: (Show a) => MVec.IOVector a -> Int -> IO String
showIOVector v n = do
  elements <- traverse (fmap show . MVec.read v) [0 .. n - 1]
  pure $ "[" ++ intercalate ", " elements ++ "]"

binOp :: (MonadState VMState m, MonadIO m) => (Chunk.Value -> Chunk.Value -> Chunk.Value) -> m ()
binOp op = do
  b <- pop
  a <- pop
  push (op a b)

interpret :: (MonadReader Chunk.Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m ()
interpret = do
  when debug $ do
    s <- get
    liftIO (showIOVector (stack s) (stackTop s) >>= putStrLn)
    debugCurrentInstruction
  op <- Chunk.fromWord <$> readByte
  case op of
    Chunk.OpReturn -> pure ()
    Chunk.OpConstant -> readConstant >>= (<* interpret) . push
    Chunk.OpNegate -> pop >>= (<* interpret) . (push . negate)
    Chunk.OpAdd -> binOp (+) <* interpret
    Chunk.OpSub -> binOp (-) <* interpret
    Chunk.OpMul -> binOp (*) <* interpret
    Chunk.OpDiv -> binOp (/) <* interpret

debugCurrentInstruction :: (MonadReader Chunk.Chunk m, MonadState VMState m, MonadIO m) => m ()
debugCurrentInstruction = do
  idx <- gets currentInstruction
  chunk <- ask
  liftIO $ Chunk.disassembleInstruction_ idx chunk

runProgram :: (MonadError LoxError m, MonadIO m) => Chunk.Chunk -> m ()
runProgram chunk = liftIO initialState >>= evalStateT (runReaderT interpret chunk)
