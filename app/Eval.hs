module Eval where

import AST (
  BinOp (..),
  Decl (..),
  Expr,
  Expr_ (..),
  Located (..),
  Program,
  Stmt (..),
  UnOp (..),
  Value (..),
  showValuePretty,
 )
import Control.Monad (void, when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Class (MonadState)
import Control.Monad.State (evalStateT, get, gets, modify, put)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Environment (Env, assign, declare, enclosing, find, newScope)
import Error (ExprError_ (..), LoxError, exprError)
import Text.Megaparsec.Pos

evalExpr :: (MonadState Env m, MonadError LoxError m) => Expr -> m Value
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
      res <- gets (find ident)
      case res of
        Just v -> pure v
        Nothing -> throwError . exprError l $ NotInScope ident
    Assgn ident e1 -> do
      v <- evalExpr e1
      res <- gets (assign ident v)
      case res of
        Nothing -> throwError . exprError l $ NotInScope ident
        Just newEnv -> put newEnv $> v
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

evalStmt :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Stmt -> m ()
evalStmt (Print e) = do
  v <- evalExpr e
  liftIO $ putStrLn (showValuePretty v)
evalStmt (EvalExpr e) = void $ evalExpr e

evalDecl :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Decl -> m ()
evalDecl (Bind i e) = do
  v <- evalExpr e
  modify (declare i v)
evalDecl (Scope program) = do
  modify newScope
  evalProgram program
  -- ok to use fromJust here because if enclosing returns `Nothing`,
  -- there's a bug in the evaluation somewhere
  modify (fromJust . enclosing)
evalDecl (Stmt s) = evalStmt s
evalDecl (If e block1 block2) = do
  -- TODO: fix, need to give decls location
  b <- expectBool id (SourcePos "" (mkPos 0) (mkPos 0)) =<< evalExpr e
  if b
    then evalProgram block1
    else for_ block2 evalProgram
evalDecl (While e block) = go
 where
  go = do
    b <- expectBool id (SourcePos "" (mkPos 0) (mkPos 0)) =<< evalExpr e
    when b $ evalProgram block *> go
evalDecl (For pre cond post block) = evalExpr pre *> innerFor
 where
  innerFor = do
    b <- expectBool id (SourcePos "" (mkPos 0) (mkPos 0)) =<< evalExpr cond
    when b $ evalProgram block *> evalExpr post *> innerFor

evalProgram :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Program -> m ()
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
