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
runRepl = evalStateT (runInputT settings repl) initialState
 where
  initialState = globalEnv
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
executeRepl input = either throwError pure (runLoxParser False "" input) >>= evalRepl_

executeProgram :: (MonadState Env m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
executeProgram input = either throwError pure (runLoxParser True "" input) >>= evalProgram_

regular :: FilePath -> IO ()
regular file = do
  t <- T.pack <$> readFile file
  res <- runExceptT (evalStateT (executeProgram t) globalEnv)
  case res of
    Left e -> print e
    Right () -> pure ()

main :: IO ()
main = do
  opts <- parseOptions
  case executionMode opts of
    Repl -> runRepl
    File file -> regular file
