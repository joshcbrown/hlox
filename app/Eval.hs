module Eval where

import AST (BinOp (..), Expr, Expr_ (..), SourcePos (..), UnOp (..), Value (..), unPos)
import Control.Applicative ((<|>))
import Data.Either (fromRight)

eval :: Expr -> Either String Value
eval (Value v _) = Right v
eval (UnOp op e l) =
  eval e
    >>= case op of
      Negate -> neg
      Not -> not'
 where
  neg v = TNum <$> expectNum negate l v
  not' v = TBool <$> expectBool not v
eval (BinOp op e1 e2 l) = do
  v1 <- eval e1
  v2 <- eval e2
  operator v1 v2
 where
  bopN op' wrap v1 v2 = wrap <$> (expectNum op' l v1 >>= \f -> expectNum f l v2)
  bopB op' wrap v1 v2 = wrap <$> (expectBool op' v1 >>= \f -> expectBool f v2)
  bopS op' wrap v1 v2 = wrap <$> (expectString op' v1 >>= \f -> expectString f v2)

  eq (TNum x) = expectNum (== x) l
  eq (TBool b) = expectBool (== b)
  eq (TString s) = expectString (== s)
  eq TNil = expectNil True

  eq' x y = either (const (Right False)) Right (eq x y)
  eq'' x y = TBool <$> eq' x y
  neq'' x y = TBool . not <$> eq' x y

  fallibleDiv x y = do
    x' <- expectNum id l x
    y' <- expectNum id l y
    if y' == 0.0
      then Left "div by zero"
      else Right . TNum $ x' / y'

  operator = case op of
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
    Dot -> \_ _ -> Left ". operator not supported"
eval _ = Left "expr not supported"

errorWith :: SourcePos -> String -> Either String a
errorWith l s =
  Left $
    "line "
      ++ show (unPos (sourceLine l))
      ++ " col "
      ++ show (unPos (sourceColumn l))
      ++ ": "
      ++ s

expectNum :: (Double -> a) -> SourcePos -> Value -> Either String a
expectNum f _ (TNum x) = return $ f x
expectNum _ l v = errorWith l $ "expected number, got: " ++ show v

expectBool :: (Bool -> a) -> Value -> Either String a
expectBool f (TBool b) = return $ f b
expectBool _ v = Left $ "expected bool, got: " ++ show v

expectString :: (String -> a) -> Value -> Either String a
expectString f (TString s) = return $ f s
expectString _ v = Left $ "expected string, got: " ++ show v

expectNil :: a -> Value -> Either String a
expectNil a TNil = return a
expectNil _ v = Left $ "expected nil, got: " ++ show v
