module Main where

import AST (Assignments, decl)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Text qualified as T
import Eval (StmtAction (..), evalDecl, evalStmt, runEvaluation)
import System.Console.Haskeline
import Text.Megaparsec (eof, errorBundlePretty, runParser)

-- TODO: make a unified error type so that this nested case shit can be squished
loop :: Assignments -> InputT IO ()
loop env = do
  line <- getInputLine "lox> "
  case line of
    Nothing -> loop env
    Just s -> do
      let parseResult = runParser (decl <* eof) "" (T.pack s)
      case parseResult of
        Left err -> outputStrLn (errorBundlePretty err) *> loop env
        Right statement -> do
          let evaluation = evalDecl statement
          res <- liftIO (runEvaluation evaluation env)
          case res of
            Left e -> outputStrLn (show e) *> loop env
            Right ((), env') -> loop env'

updateEnv :: StmtAction -> Assignments -> Assignments
updateEnv (Assign s v) = M.insert s v
updateEnv _ = id

main :: IO ()
main = runInputT defaultSettings (loop M.empty)
