-- | Special Constants
module Ily.Syntax.Const where

data SCons
  = SInt Integer
  | SStr String
  | SChar Char
  deriving (Eq, Ord, Show)


