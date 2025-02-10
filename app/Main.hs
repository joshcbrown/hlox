module Main where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, MonadTrans (lift), evalStateT)
import Data.Text qualified as T
import Environment (Env, empty)
import Error (LoxError, runLoxParser)
import Eval (evalProgram)
import Opts (ExecutionMode (..), executionMode, parseOptions)
import System.Console.Haskeline

runRepl :: IO ()
runRepl = evalStateT (runInputT settings repl) initialState
 where
  initialState = empty
  settings =
    defaultSettings
      { historyFile = Just ".lox_history"
      }

repl :: (MonadState Env m, MonadIO m, MonadMask m) => InputT m ()
repl = do
  minput <- getInputLine "lox> "
  case minput of
    Nothing -> return ()
    Just "exit" -> return ()
    Just input -> do
      res <- lift $ runExceptT (executeProgram (T.pack input))
      case res of
        Left e -> liftIO $ print e
        _ -> pure ()
      repl

executeProgram :: (MonadState Env m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
executeProgram input = do
  program <- either throwError pure $ runLoxParser "" input
  evalProgram program

regular :: FilePath -> IO ()
regular file = do
  t <- T.pack <$> readFile file
  res <- runExceptT (evalStateT (executeProgram t) empty)
  case res of
    Left e -> print e
    Right () -> pure ()

main :: IO ()
main = do
  opts <- parseOptions
  case executionMode opts of
    Repl -> runRepl
    File file -> regular file
