module Main where

import AST (expr)
import Data.Text qualified as T
import Eval (eval)
import Text.Megaparsec (eof, errorBundlePretty, runParser)

tryEval :: T.Text -> IO ()
tryEval t = do
  let parseResult = runParser (expr <* eof) "" t
  putStrLn $ case parseResult of
    Left e -> errorBundlePretty e
    Right e -> show $ eval e

main :: IO ()
main = putStrLn "Hello, Haskell!"
