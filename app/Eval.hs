module Eval where

import AST (BinOp (..), Expr (..), UnOp (..), Value (..))

eval :: Expr -> Either String Value
eval (Value v) = Right v
eval (UnOp op e) =
  eval e
    >>= case op of
      Negate -> neg
      Not -> not'
 where
  neg v = TNum <$> expectNum negate v
  not' v = TBool <$> expectBool not v
eval (BinOp op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  operator v1 v2
 where
  bopN op' wrap v1 v2 = wrap <$> (expectNum op' v1 >>= \f -> expectNum f v2)
  bopB op' wrap v1 v2 = wrap <$> (expectBool op' v1 >>= \f -> expectBool f v2)

  eq (TNum x) = expectNum (== x)
  eq (TBool b) = expectBool (== b)
  eq (TString s) = expectString (== s)
  eq TNil = expectNil True

  eq' x y = TBool <$> eq x y
  neq' x y = TBool . not <$> eq x y

  operator = case op of
    Add -> bopN (+) TNum
    Sub -> bopN (-) TNum
    Mul -> bopN (*) TNum
    Div -> bopN (/) TNum
    Lt -> bopN (<) TBool
    Leq -> bopN (<=) TBool
    Gt -> bopN (>) TBool
    Geq -> bopN (>=) TBool
    Or -> bopB (||) TBool
    And -> bopB (&&) TBool
    Eq -> eq'
    Neq -> neq'
    Dot -> \_ _ -> Left ". operator not supported"
eval _ = Left "expr not supported"

expectNum :: (Double -> a) -> Value -> Either String a
expectNum f (TNum x) = return $ f x
expectNum _ v = Left $ "expected number, got: " ++ show v

expectBool :: (Bool -> a) -> Value -> Either String a
expectBool f (TBool b) = return $ f b
expectBool _ v = Left $ "expected bool, got: " ++ show v

expectString :: (String -> a) -> Value -> Either String a
expectString f (TString s) = return $ f s
expectString _ v = Left $ "expected string, got: " ++ show v

expectNil :: a -> Value -> Either String a
expectNil a TNil = return a
expectNil _ v = Left $ "expected nil, got: " ++ show v
