-- | Special Constants
module Ily.Syntax.Const where

data SCons
  = SInt Integer
  | SStr String
  | SChar Char
  | SBool Bool
  deriving (Eq, Ord, Show)

true :: SCons
true = SBool True

false :: SCons
false = SBool False
