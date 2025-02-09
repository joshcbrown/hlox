{-# LANGUAGE DerivingStrategies #-}

module Eval where

import AST (
  Assignments,
  BinOp (..),
  Decl (..),
  Expr,
  Expr_ (..),
  Located (..),
  Program,
  SourcePos (..),
  Stmt (..),
  UnOp (..),
  Value (..),
  showValuePretty,
 )
import Control.Monad (void)
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Class (MonadState)
import Control.Monad.State (get, modify)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Error (ExprError_ (..), LoxError, exprError)

evalExpr :: (MonadState Assignments m, MonadError LoxError m) => Expr -> m Value
evalExpr (Located l e) =
  case e of
    Value v -> pure v
    UnOp op e1 ->
      evalExpr e1
        >>= case op of
          Negate -> neg
          Not -> not'
    BinOp op e1 e2 -> do
      v1 <- evalExpr e1
      v2 <- evalExpr e2
      operator op v1 v2
    Ident ident -> do
      env <- get
      case M.lookup ident env of
        Just v -> pure v
        Nothing -> throwError . exprError l $ NotInScope ident
    Assgn i e1 -> do
      v <- evalExpr e1
      modify (M.insert i v)
      pure v
 where
  neg v = TNum <$> expectNum negate l v
  not' v = TBool <$> expectBool not l v
  bopN op wrap v1 v2 = wrap <$> (expectNum op l v1 >>= \f -> expectNum f l v2)
  bopB op wrap v1 v2 = wrap <$> (expectBool op l v1 >>= \f -> expectBool f l v2)

  eq (TNum x) (TNum y) = x == y
  eq (TBool b1) (TBool b2) = b1 == b2
  eq (TString s1) (TString s2) = s1 == s2
  eq TNil TNil = True
  eq _ _ = False

  eq' x y = pure . TBool $ eq x y
  neq' x y = pure . TBool . not $ eq x y

  fallibleDiv x y = do
    x' <- expectNum id l x
    y' <- expectNum id l y
    if y' == 0.0
      then throwError . exprError l $ DivideByZero
      else pure . TNum $ x' / y'

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
    Eq -> eq'
    Neq -> neq'
    Dot -> \_ _ -> throwError . exprError l $ NotSupportedError ". operator"

evalStmt :: (MonadState Assignments m, MonadError LoxError m, MonadIO m) => Stmt -> m ()
evalStmt (Print e) = do
  v <- evalExpr e
  liftIO $ putStrLn (showValuePretty v)
evalStmt (EvalExpr e) = void $ evalExpr e

evalDecl :: (MonadState Assignments m, MonadError LoxError m, MonadIO m) => Decl -> m ()
evalDecl (Bind i e) = do
  v <- evalExpr e
  modify (M.insert i v)
evalDecl (Stmt s) = evalStmt s

evalProgram :: (MonadState Assignments m, MonadError LoxError m, MonadIO m) => Program -> m ()
evalProgram = traverse_ evalDecl

expectNum :: (MonadError LoxError m) => (Double -> a) -> SourcePos -> Value -> m a
expectNum f _ (TNum x) = return $ f x
expectNum _ l v = throwError . exprError l $ TypeError "num" v

expectBool :: (MonadError LoxError m) => (Bool -> a) -> SourcePos -> Value -> m a
expectBool f _ (TBool b) = return $ f b
expectBool _ l v = throwError . exprError l $ TypeError "bool" v

expectString :: (MonadError LoxError m) => (String -> a) -> SourcePos -> Value -> m a
expectString f _ (TString s) = return $ f s
expectString _ l v = throwError . exprError l $ TypeError "string" v

expectNil :: (MonadError LoxError m) => a -> SourcePos -> Value -> m a
expectNil a _ TNil = return a
expectNil _ l v = throwError . exprError l $ TypeError "nil" v
