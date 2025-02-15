module Main where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, MonadTrans (lift), evalStateT)
import Data.Text qualified as T
import Environment (Env)
import Eval (evalProgram_, evalRepl_)
import LoxPrelude (globalEnv)
import Opts (ExecutionMode (..), executionMode, parseOptions)
import Parse (runLoxParser)
import System.Console.Haskeline
import Types (LoxError)

runRepl :: IO ()
runRepl = globalEnv >>= evalStateT (runInputT settings repl)
 where
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
      res <- lift $ runExceptT (executeRepl (T.pack input))
      case res of
        Left e -> liftIO $ print e
        _ -> pure ()
      repl

executeRepl :: (MonadState Env m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
executeRepl input = either throwError pure (runLoxParser "" input) >>= evalRepl_

executeProgram :: (MonadState Env m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
executeProgram input = either throwError pure (runLoxParser "" input) >>= evalProgram_

regular :: FilePath -> IO ()
regular file = do
  t <- T.pack <$> readFile file
  global <- globalEnv
  res <- runExceptT (evalStateT (executeProgram t) global)
  case res of
    Left e -> print e
    Right () -> pure ()

main :: IO ()
main = do
  opts <- parseOptions
  case executionMode opts of
    Repl -> runRepl
    File file -> regular file
