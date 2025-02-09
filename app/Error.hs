module Error (
  LoxError,
  ExprError_ (..),
  exprError,
  runLoxParser,
) where

import AST (Located (..), Program, Value, program)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

data LoxError = SyntaxError (ParseErrorBundle Text Void) | RuntimeError ExprError

instance Show LoxError where
  show (SyntaxError b) = "syntax error:\n" <> errorBundlePretty b
  show (RuntimeError e) = "runtime error:\n" <> show e

data ExprError_
  = TypeError
      { -- think this needs to be stringly typed because eventually well want
        -- support for user defined classes
        expected :: String
      , got :: Value
      }
  | DivideByZero
  | NotInScope
      {var :: String}
  | NotSupportedError
      {unsupported :: String}

instance Show ExprError_ where
  show (TypeError expected got) = "expected " ++ expected ++ ", got " ++ show got
  show DivideByZero = "divide by zero"
  show (NotSupportedError op) = op ++ " not supported"
  show (NotInScope ident) = "variable " ++ ident ++ " does not exist"

type ExprError = Located ExprError_

exprError :: SourcePos -> ExprError_ -> LoxError
exprError l e = RuntimeError $ Located l e

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

runLoxParser :: String -> Text -> Either LoxError Program
runLoxParser fname input = mapLeft SyntaxError $ runParser program fname input
