module Eval where

import Control.Applicative ((<|>))
import Control.Monad (join, void, when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Class (MonadState)
import Control.Monad.State (gets, modify, put)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Data.IORef (readIORef)
import Data.Maybe (fromJust, fromMaybe)
import Environment (Env, assign, declare, enclosing, find, newScope, printState)
import Text.Megaparsec.Pos
import Types (
  BinOp (..),
  Decl (..),
  Expr,
  ExprError_ (..),
  Expr_ (..),
  Located (..),
  LoxError,
  Program,
  UnOp (..),
  Value (..),
  exprError,
 )

evalExpr :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Expr -> m Value
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
      case op of
        -- manual short circuit
        And -> do
          b1 <- expectBool id (location e1) v1
          if b1
            then
              TBool <$> (evalExpr e2 >>= expectBool id (location e2))
            else pure (TBool False)
        Or -> do
          notB1 <- expectBool not (location e1) v1
          if notB1
            then
              TBool <$> (evalExpr e2 >>= expectBool id (location e2))
            else pure (TBool True)
        _ -> do
          v2 <- evalExpr e2
          operator op v1 v2
    Ident ident -> do
      res <- gets (find ident)
      case res of
        Just ref -> liftIO $ readIORef ref
        Nothing -> throwError . exprError l $ NotInScope ident
    Assgn ident e1 -> do
      v <- evalExpr e1
      res <- gets (assign ident v)
      res' <- liftIO res
      case res' of
        Nothing -> throwError . exprError l $ NotInScope ident
        Just newEnv -> put newEnv $> v
    Call fExpr argsExpr -> do
      f <- evalExpr fExpr
      args <- traverse evalExpr argsExpr
      expectCallable args l f
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

nothing :: (MonadState Env m, MonadError LoxError m, MonadIO m) => m b -> m (Maybe a)
nothing m = m $> Nothing

declare' :: (MonadState Env m, MonadIO m) => String -> Value -> m ()
declare' s v = do
  currEnv <- gets id
  newEnv <- liftIO $ declare s v currEnv
  put newEnv

evalDecl :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Decl -> m (Maybe Value)
evalDecl (Bind i e) = nothing $ evalExpr e >>= declare' i
evalDecl (Scope program) = evalProgram program
evalDecl (EvalExpr e) = nothing $ evalExpr e
evalDecl (If cond block1 block2) = do
  b <- evalCond cond
  if b
    then evalProgram block1
    else maybe (pure Nothing) evalProgram block2
evalDecl (While cond block) = nothing loop
 where
  loop = do
    b <- evalCond cond
    when b $ evalProgram block *> loop
evalDecl (Fun name params body) = nothing $ declare' name (TFunction name params body)
evalDecl (Return e) = Just <$> evalExpr e

evalRepl :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Program -> m (Maybe Value)
evalRepl (d : ds) = do
  r1 <- evalDecl d
  -- seems we can't use <|> :(
  case r1 of
    Nothing -> evalRepl ds
    Just r -> pure $ Just r
evalRepl [] = pure Nothing

evalRepl_ :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Program -> m ()
evalRepl_ = void . evalRepl

evalProgram :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Program -> m (Maybe Value)
evalProgram prog = inNewScope $ evalRepl prog

evalProgram_ :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Program -> m ()
evalProgram_ = void . inNewScope . evalRepl

evalCond :: (MonadError LoxError m, MonadState Env m, MonadIO m) => Expr -> m Bool
evalCond cond = expectBool id (location cond) =<< evalExpr cond

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

logState :: (MonadState Env m, MonadIO m) => String -> m ()
logState state = do
  s <- gets id
  liftIO (putStrLn ("STATE " ++ state ++ "\n") *> printState s *> putStrLn "\n\n")

inNewScope :: (MonadState Env m, MonadError LoxError m, MonadIO m) => m a -> m a
inNewScope m = modify newScope *> m <* modify (fromJust . enclosing)

expectCallable :: (MonadError LoxError m, MonadState Env m, MonadIO m) => [Value] -> SourcePos -> Value -> m Value
expectCallable args l (TNativeFunction f) = liftIO (f args l) >>= either throwError pure
expectCallable args l (TFunction name params body) = do
  if length args /= length params
    then throwError . exprError l $ Arity name (length params) (length args)
    else inNewScope $ do
      traverse_ (uncurry declare') (zip params args)
      fromMaybe TNil <$> evalRepl body
expectCallable _ l v = throwError . exprError l $ TypeError "callable" v
