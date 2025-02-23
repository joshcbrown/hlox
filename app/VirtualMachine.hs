module VirtualMachine where

import Chunk qualified
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, asks, runReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify, put)
import Data.ByteString qualified as BS
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
initialState = VMState 0 0 <$> MVec.replicate 0 1024

-- TODO: check for stack overflow
push :: Chunk.Value -> VMState -> IO VMState
push value s@(VMState _ stackTop stack) = do
  MVec.write stack stackTop value
  pure s{stack = stack, stackTop = stackTop + 1}

-- this can be pure because we don't need to update the underlying vec
pop :: VMState -> VMState
pop s = s{stackTop = stackTop s + 1}

modifyInstruction :: (MonadState VMState m) => Int -> m ()
modifyInstruction i = modify (\vm -> vm{currentInstruction = i})

readByte :: (MonadReader Chunk.Chunk m, MonadState VMState m) => m Word8
readByte = do
  idx <- gets currentInstruction
  modifyInstruction (idx + 1) *> asks (Chunk.readWord idx)

readConstant :: (MonadReader Chunk.Chunk m, MonadState VMState m) => m Chunk.Value
readConstant = readByte >>= (asks . Chunk.getValue) . fromIntegral

interpret :: (MonadReader Chunk.Chunk m, MonadState VMState m, MonadError LoxError m, MonadIO m) => m ()
interpret = do
  when debug $ do
    stack <- gets stack
    debugCurrentInstruction
  op <- Chunk.fromWord <$> readByte
  case op of
    Chunk.OpReturn -> pure ()
    Chunk.OpConstant -> do
      c <- readConstant
      liftIO $ putStrLn (Chunk.valuePretty c)
      interpret

debugCurrentInstruction :: (MonadReader Chunk.Chunk m, MonadState VMState m, MonadIO m) => m ()
debugCurrentInstruction = do
  idx <- gets currentInstruction
  chunk <- ask
  liftIO $ Chunk.disassembleInstruction_ idx chunk

runProgram :: Chunk.Chunk -> IO (Either LoxError ())
runProgram chunk = initialState >>= runExceptT . evalStateT (runReaderT interpret chunk)
