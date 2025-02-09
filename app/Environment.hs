module Environment (Env, Scope, find, declare, assign, empty, newScope, enclosing)
where

import Data.Map qualified as M

import AST (Value)
import Control.Applicative ((<|>))
import Data.Functor (($>))

type Scope = M.Map String Value

data Env = Global Scope | Local Env Scope
  deriving (Show)

empty :: Env
empty = Global M.empty

newScope :: Env -> Env
newScope env = Local env M.empty

enclosing :: Env -> Maybe Env
enclosing (Global _) = Nothing
enclosing (Local e _) = Just e

find :: String -> Env -> Maybe Value
find s (Global scope) = M.lookup s scope
find s (Local enc scope) = M.lookup s scope <|> find s enc

modifyScope :: (Scope -> Scope) -> Env -> Env
modifyScope f (Global scope) = Global $ f scope
modifyScope f (Local enc scope) = Local enc (f scope)

declare :: String -> Value -> Env -> Env
declare s v = modifyScope (M.insert s v)

assign :: String -> Value -> Env -> Maybe Env
assign s v env@(Global scope) = M.lookup s scope $> declare s v env
assign s v env@(Local enclosing scope) =
  (M.lookup s scope $> declare s v env)
    <|> (flip Local scope <$> assign s v enclosing)
