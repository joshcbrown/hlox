{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Class (MonadState)
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Map qualified as M

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

data StmtAction = NoAction | Assign String Value
  deriving (Show)
type StmtResult = Either ExprError StmtAction

-- TODO: define effect for printing, use instead of MonadIO
newtype Evaluation env err a = Evaluation (StateT env (ExceptT err IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadState env, MonadError err, MonadIO)

runEvaluation :: Evaluation env err a -> env -> IO (Either err (a, env))
runEvaluation (Evaluation m) initialEnv = runExceptT (runStateT m initialEnv)

evalExpr :: (MonadState Assignments m, MonadError ExprError m) => Expr -> m Value
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
        Nothing -> throwError $ Located l $ NotInScope ident
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
      then throwError . Located l $ DivideByZero
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
    Dot -> \_ _ -> throwError $ Located l $ NotSupportedError ". operator"

evalStmt :: (MonadState Assignments m, MonadError ExprError m, MonadIO m) => Stmt -> m ()
evalStmt (Print e) = do
  v <- evalExpr e
  liftIO $ putStrLn (showValuePretty v)
evalStmt (EvalExpr e) = void $ evalExpr e

evalDecl :: (MonadState Assignments m, MonadError ExprError m, MonadIO m) => Decl -> m ()
evalDecl (Bind i e) = do
  v <- evalExpr e
  modify (M.insert i v)
evalDecl (Stmt s) = evalStmt s

expectNum :: (MonadError ExprError m) => (Double -> a) -> SourcePos -> Value -> m a
expectNum f _ (TNum x) = return $ f x
expectNum _ l v = throwError $ Located l $ TypeError "num" v

expectBool :: (MonadError ExprError m) => (Bool -> a) -> SourcePos -> Value -> m a
expectBool f _ (TBool b) = return $ f b
expectBool _ l v = throwError $ Located l $ TypeError "bool" v

expectString :: (MonadError ExprError m) => (String -> a) -> SourcePos -> Value -> m a
expectString f _ (TString s) = return $ f s
expectString _ l v = throwError $ Located l $ TypeError "string" v

expectNil :: (MonadError ExprError m) => a -> SourcePos -> Value -> m a
expectNil a _ TNil = return a
expectNil _ l v = throwError $ Located l $ TypeError "nil" v
