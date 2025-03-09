module AST where

import Types (Located)

data BinOp = Add | Sub | Mul | Div | Or | And | Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show, Enum)
data UnOp = Negate | Not
  deriving (Show)

data Value
  = TNum Double
  | TString String
  | TBool Bool
  | TNil
  | TFunction String [String] [Decl]
  deriving (Show)

showValuePretty :: Value -> String
showValuePretty (TNum x) = show x
showValuePretty (TString s) = show s
showValuePretty (TBool b) = show b
showValuePretty TNil = "nil"
showValuePretty (TFunction name _ _) = "<function " ++ name ++ ">"

data Expr_
  = Ident String
  | Value Value
  | Assgn String Expr
  | UnOp UnOp Expr
  | BinOp BinOp Expr Expr
  | Call Expr [Expr]
  deriving (Show)

data Stmt
  = EvalExpr Expr
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Return Expr
  | Scope Program
  deriving (Show)

data Decl
  = Bind String Expr
  | Fun String [String] Program
  | EvalStmt Stmt
  deriving (Show)

type Expr = Located Expr_
type Program = [Decl]
