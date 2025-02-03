module Eval where

import AST (
  Assignments,
  BinOp (..),
  Decl (..),
  Expr,
  Expr_ (..),
  Located (..),
  SourcePos (..),
  Stmt (..),
  UnOp (..),
  Value (..),
  showValuePretty,
 )
import Control.Monad (void)
import Data.Functor (($>))
import Data.Map qualified as M

data ExprError_
  = TypeError
      { -- think this needs to be stringly typed because eventually we'll want
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

data StmtAction = NoAction | Assign String Value
  deriving (Show)
type StmtResult = Either ExprError StmtAction

evalExpr :: Expr -> Assignments -> Either ExprError Value
evalExpr (Located l e) env =
  case e of
    Value v -> Right v
    UnOp op e1 ->
      evalExpr e1 env
        >>= case op of
          Negate -> neg
          Not -> not'
    BinOp op e1 e2 -> do
      v1 <- evalExpr e1 env
      v2 <- evalExpr e2 env
      operator op v1 v2
    Ident i -> case M.lookup i env of
      Just v -> Right v
      Nothing -> Left . Located l $ NotInScope i
 where
  neg v = TNum <$> expectNum negate l v
  not' v = TBool <$> expectBool not l v
  bopN op' wrap v1 v2 = wrap <$> (expectNum op' l v1 >>= \f -> expectNum f l v2)
  bopB op' wrap v1 v2 = wrap <$> (expectBool op' l v1 >>= \f -> expectBool f l v2)

  eq (TNum x) = expectNum (== x) l
  eq (TBool b) = expectBool (== b) l
  eq (TString s) = expectString (== s) l
  eq TNil = expectNil True l

  eq' x y = either (const (Right False)) Right (eq x y)
  eq'' x y = TBool <$> eq' x y
  neq'' x y = TBool . not <$> eq' x y

  fallibleDiv x y = do
    x' <- expectNum id l x
    y' <- expectNum id l y
    if y' == 0.0
      then Left . Located l $ DivideByZero
      else Right . TNum $ x' / y'

  operator op = case op of
    Add -> bopN (+) TNum
    Sub -> bopN (-) TNum
    Mul -> bopN (*) TNum
    Div -> fallibleDiv
    Lt -> bopN (<) TBool
    Leq -> bopN (<=) TBool
    Gt -> bopN (>) TBool
    Geq -> bopN (>=) TBool
    Or -> bopB (||) TBool
    And -> bopB (&&) TBool
    Eq -> eq''
    Neq -> neq''
    Dot -> \_ _ -> Left $ Located l $ NotSupportedError ". operator"

evalStmt :: Stmt -> Assignments -> IO StmtResult
evalStmt (Print e) env =
  let evalRes = evalExpr e env
   in case evalRes of
        Left err -> print e $> Left err
        Right v -> putStrLn (showValuePretty v) $> Right NoAction
evalStmt (EvalExpr e) env = return (NoAction <$ evalExpr e env)

evalDecl :: Decl -> Assignments -> IO StmtResult
evalDecl (Bind i e) env =
  let evalRes = evalExpr e env
   in case evalRes of
        Left err -> return $ Left err
        Right v -> return . Right $ Assign i v
evalDecl (Stmt s) env = evalStmt s env

expectNum :: (Double -> a) -> SourcePos -> Value -> Either ExprError a
expectNum f _ (TNum x) = return $ f x
expectNum _ l v = Left $ Located l $ TypeError "num" v

expectBool :: (Bool -> a) -> SourcePos -> Value -> Either ExprError a
expectBool f _ (TBool b) = return $ f b
expectBool _ l v = Left $ Located l $ TypeError "bool" v

expectString :: (String -> a) -> SourcePos -> Value -> Either ExprError a
expectString f _ (TString s) = return $ f s
expectString _ l v = Left $ Located l $ TypeError "string" v

expectNil :: a -> SourcePos -> Value -> Either ExprError a
expectNil a _ TNil = return a
expectNil _ l v = Left $ Located l $ TypeError "nil" v
