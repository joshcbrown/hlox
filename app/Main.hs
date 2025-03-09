module Main where

import Chunk
import Compiler (compileProgram_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, MonadTrans (lift), evalStateT)
import Data.Text qualified as T
import Error
import Opts (ExecutionMode (..), executionMode, parseOptions)
import Parse (runLoxParser)
import System.Console.Haskeline
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
    c <- liftEither $ compileProgram_ d
    liftIO $ disassembleChunk c
    VM.runProgram c

executeProgram :: (MonadState VM.VMState m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
executeProgram input =
  liftEither (runLoxParser "" input)
    >>= liftEither . compileProgram_
    >>= VM.runProgram

regular :: FilePath -> IO ()
regular file = do
  t <- T.pack <$> readFile file
  initialState <- VM.initialState
  res <- runExceptT (evalStateT (executeProgram t) initialState)
  case res of
    Left e -> print e
    Right () -> pure ()

main :: IO ()
main = do
  opts <- parseOptions
  case executionMode opts of
    Repl -> runRepl
    File file -> regular file
