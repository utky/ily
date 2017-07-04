module Ily.Nel where

import qualified Data.Foldable as F

-- | Non empty list
data Nel a
  = Cons a (Nel a)
  | Last a
  deriving (Eq, Show)

instance F.Foldable Nel where
  F.foldr f i (Last x) = f x i
  F.foldr f i (Cons x xs) = f x (F.foldr f i xs)

