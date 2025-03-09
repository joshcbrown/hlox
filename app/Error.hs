module Error where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Types (Located (..))

data LoxError = SyntaxError (ParseErrorBundle Text Void) | CompilerError CompilerError | RuntimeError ExprError

instance Show LoxError where
  show (SyntaxError b) = "syntax error:\n" <> errorBundlePretty b
  show (RuntimeError e) = "runtime error:\n" <> show e
  show (CompilerError e) = "compile time error:\n" <> show e

data ExprError_
  = TypeError
      { -- think this needs to be stringly typed because eventually well want
        -- support for user defined classes
        expected :: String
      , got :: String
      }
  | DivideByZero
  | NotInScope
      {var :: String}
  | NotSupportedError
      {unsupported :: String}
  | Arity
      { callSite :: String
      , expectedArity :: Int
      , gotArity :: Int
      }

data CompileError_
  = Shadowing String
  | SelfRefInDecl String
  deriving (Show)

type CompilerError = Located CompileError_

compilerError :: SourcePos -> CompileError_ -> LoxError
compilerError l e = CompilerError $ Located l e

instance Show ExprError_ where
  show (TypeError expected got) = "expected " ++ expected ++ ", got " ++ show got
  show DivideByZero = "divide by zero"
  show (NotSupportedError op) = op ++ " not supported"
  show (NotInScope ident) = "variable " ++ ident ++ " does not exist"
  show (Arity callSite expected got) =
    "incorrect arity: function "
      ++ callSite
      ++ " accepts "
      ++ show expected
      ++ "arguments, got "
      ++ show got

type ExprError = Located ExprError_

exprError :: SourcePos -> ExprError_ -> LoxError
exprError l e = RuntimeError $ Located l e

syntaxError :: ParseErrorBundle Text Void -> LoxError
syntaxError = SyntaxError
