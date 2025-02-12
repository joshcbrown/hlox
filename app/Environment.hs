module Environment -- (Env, Scope, find, declare, assign, empty, newScope, enclosing)
where

import Data.Map qualified as M

import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Functor (($>))
import Types (Value)

type Scope = M.Map String Value

newtype Env_ a = Env [a]
  deriving (Show, Functor)

type Env = Env_ Scope

attach :: Scope -> Env -> Env
attach s (Env ss) = Env (s : ss)

empty :: Env
empty = Env []

newScope :: Env -> Env
newScope (Env scopes) = Env $ M.empty : scopes

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : xs) = Just xs

enclosing :: Env -> Maybe Env
enclosing (Env scopes) = Env <$> tail' scopes

find :: String -> Env -> Maybe Value
find i (Env scopes) = asum $ M.lookup i <$> scopes

-- this is a bit sussy
modifyScope :: (Scope -> Scope) -> Env -> Env
modifyScope f (Env (scope : scopes)) = Env $ f scope : scopes
modifyScope _ (Env []) = Env []

declare :: String -> Value -> Env -> Env
declare s v = modifyScope (M.insert s v)

global :: M.Map String Value -> Env
global = Env . (: [])

assign :: String -> Value -> Env -> Maybe Env
assign _ _ (Env []) = Nothing
assign s v env@(Env (scope : scopes)) =
  (M.lookup s scope $> declare s v env)
    <|> (attach scope <$> assign s v (Env scopes))
