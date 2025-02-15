module Environment -- (Env, Scope, find, declare, assign, empty, newScope, enclosing)
where

import Data.Map qualified as M

import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.IORef
import Types (Value)

type Scope = M.Map String (IORef Value)

newtype Env_ a = Env [a]
  deriving (Show, Functor, Foldable, Traversable)

type Env = Env_ Scope

printState :: Env -> IO ()
printState env = mapM (traverse readIORef) env >>= print

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

find :: String -> Env -> Maybe (IORef Value)
find i (Env scopes) = asum $ M.lookup i <$> scopes

-- this is a bit sussy
modifyScope :: (Scope -> Scope) -> Env -> Env
modifyScope f (Env (scope : scopes)) = Env $ f scope : scopes
modifyScope _ (Env []) = Env []

declare :: String -> Value -> Env -> IO Env
declare s v env = do
  ref <- newIORef v
  pure $ modifyScope (M.insert s ref) env

global :: M.Map String Value -> IO Env
global m = Env . (: []) <$> traverse newIORef m

assign :: String -> Value -> Env -> IO (Maybe Env)
assign _ _ (Env []) = return Nothing
assign s v env@(Env (scope : scopes)) = do
  case M.lookup s scope of
    Just ref -> do
      writeIORef ref v
      return $ Just env
    Nothing -> do
      maybeEnv <- assign s v (Env scopes)
      pure $ attach scope <$> maybeEnv
