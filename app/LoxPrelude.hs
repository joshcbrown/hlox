module LoxPrelude where

import Data.Functor (($>))
import Data.Int
import Data.Map qualified as M
import Data.Time.Clock.POSIX
import Environment
import Types (ExprError_ (..), NativeFunction, Value (..), exprError, showValuePretty)

clock :: NativeFunction
clock [] _ = do
  Right . TNum . fromIntegral . round . (* 1000) <$> getPOSIXTime
clock args l = pure (Left $ exprError l $ Arity "clock" 0 (length args))

printy :: NativeFunction
printy [] _ = putStrLn "" $> Right TNil
printy (x : xs) l = putStr (showValuePretty x ++ " ") *> printy xs l

globalEnv :: Env
globalEnv = global $ M.fromList [("clock", TNativeFunction clock), ("print", TNativeFunction printy)]
