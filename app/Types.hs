module Types (
  LoxError,
  BinOp (..),
  UnOp (..),
  Value (..),
  Expr_ (..),
  Expr,
  SourcePos (..),
  unPos,
  Located (..),
  Decl (..),
  Program,
  ExprError_ (..),
  CompileError_ (..),
  NativeFunction,
  compilerError,
  exprError,
  syntaxError,
  showValuePretty,
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

data BinOp = Add | Sub | Mul | Div | Or | And | Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show, Enum)
data UnOp = Negate | Not
  deriving (Show)

type NativeFunction = [Value] -> SourcePos -> IO (Either LoxError Value)

instance Show NativeFunction where
  show _ = "<native function>"

data Value
  = TNum Double
  | TString String
  | TBool Bool
  | TNil
  | TNativeFunction NativeFunction
  | TFunction String [String] [Decl]
  deriving (Show)

data Expr_
  = Ident String
  | Value Value
  | Assgn String Expr
  | UnOp UnOp Expr
  | BinOp BinOp Expr Expr
  | Call Expr [Expr]
  deriving (Show)

data Located a = Located
  { location :: SourcePos
  , unLocate :: a
  }
  deriving (Functor)

data Decl
  = Bind String Expr
  | Return Expr
  | Scope [Decl]
  | EvalExpr Expr
  | If Expr [Decl] (Maybe [Decl])
  | While Expr [Decl]
  | Fun String [String] [Decl]
  deriving (Show)

type Expr = Located Expr_
type Program = [Decl]

instance (Show a) => Show (Located a) where
  show (Located l a) = "(" ++ sourcePosPretty l ++ " " ++ show a ++ ")"

showValuePretty :: Value -> String
showValuePretty (TNum x) = show x
showValuePretty (TString s) = show s
showValuePretty (TBool b) = show b
showValuePretty TNil = "nil"
showValuePretty (TNativeFunction _) = "<native function>"
showValuePretty (TFunction name _ _) = "<function " ++ name ++ ">"

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
      , got :: Value
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
