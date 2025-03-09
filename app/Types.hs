module Types (
  Located (..),
  SourcePos (..),
  unPos,
) where

import Text.Megaparsec

data Located a = Located
  { location :: SourcePos
  , unLocate :: a
  }
  deriving (Functor)

instance (Show a) => Show (Located a) where
  show (Located l a) = "(" ++ sourcePosPretty l ++ " " ++ show a ++ ")"
