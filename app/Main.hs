module Main where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, MonadTrans (lift), evalStateT)
import Data.Text qualified as T
import Environment (Env, empty)
import Error (LoxError, runLoxParser)
import Eval (evalProgram)
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
      res <- lift $ runExceptT (processLine (T.pack input))
      case res of
        Left e -> liftIO $ print e
        _ -> pure ()
      repl

processLine :: (MonadState Env m, MonadError LoxError m, MonadIO m) => T.Text -> m ()
processLine input = do
  program <- either throwError return $ runLoxParser "" input
  evalProgram program

main :: IO ()
main = runRepl
