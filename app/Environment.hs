module Environment (Env, Scope, find, declare, assign, empty, newScope, enclosing)
where

import Data.Map qualified as M

import AST (Value)
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE

type Scope = M.Map String Value

newtype Env_ a = Env (NonEmpty a)
  deriving (Show, Functor)

type Env = Env_ Scope

attach :: Scope -> Env -> Env
attach s (Env ss) = Env (s <| ss)

empty :: Env
empty = Env $ singleton M.empty

newScope :: Env -> Env
newScope (Env scopes) = Env $ M.empty <| scopes

enclosing :: Env -> Maybe Env
enclosing (Env scopes) = Env <$> (nonEmpty . NE.tail $ scopes)

find :: String -> Env -> Maybe Value
find i (Env scopes) = asum $ M.lookup i <$> scopes

modifyScope :: (Scope -> Scope) -> Env -> Env
modifyScope f (Env (scope :| scopes)) = Env $ f scope :| scopes

declare :: String -> Value -> Env -> Env
declare s v = modifyScope (M.insert s v)

assign :: String -> Value -> Env -> Maybe Env
assign s v env@(Env (scope :| [])) = M.lookup s scope $> declare s v env
assign s v env@(Env (scope :| scopes)) =
  (M.lookup s scope $> declare s v env)
    <|> (attach scope <$> assign s v (Env $ NE.fromList scopes))
