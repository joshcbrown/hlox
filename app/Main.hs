module Main where

import Chunk
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, MonadTrans (lift), evalStateT)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Environment (Env)
import LoxPrelude (globalEnv)
import Opts (ExecutionMode (..), executionMode, parseOptions)
import Parse (decl, runLoxParser)
import System.Console.Haskeline
import Types (LoxError)
import VirtualMachine qualified as VM

runRepl :: IO ()
runRepl = VM.initialState >>= evalStateT (runInputT settings repl)
 where
  settings =
    defaultSettings
      { historyFile = Just ".lox_history"
      }

repl :: (MonadIO m, MonadMask m, MonadState VM.VMState m) => InputT m ()
repl = do
  minput <- getInputLine "lox> "
  case minput of
    Nothing -> return ()
    Just "exit" -> return ()
    Just input -> do
      res <- lift $ runExceptT (executeRepl (T.pack input))
      case res of
        Left e -> liftIO $ print e
        _ -> pure ()
      lift VM.resetVM
      repl

executeRepl :: (MonadError LoxError m, MonadIO m, MonadState VM.VMState m) => T.Text -> m ()
executeRepl input =
  liftEither (runLoxParser "" input) >>= \d -> do
    c <- liftEither $ Chunk.fromProgram_ d
    liftIO $ disassembleChunk c
    VM.runProgram c

-- old
-- executeProgram :: (MonadState Env m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
-- executeProgram input = either throwError pure (runLoxParser True "" input) >>= evalProgram_
--
-- regular :: FilePath -> IO ()
-- regular file = do
--   t <- T.pack <$> readFile file
--   res <- runExceptT (evalStateT (executeProgram t) globalEnv)
--   case res of
--     Left e -> print e
--     Right () -> pure ()

-- main :: IO ()
-- main = do
--   opts <- parseOptions
--   case executionMode opts of
--     Repl -> runRepl
--     File file -> regular file

-- new (temporary)

main :: IO ()
main = do
  runRepl
