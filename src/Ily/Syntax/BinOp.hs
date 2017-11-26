-- | Defines pre-defined binary operators
module Ily.Syntax.BinOp where

data Assoc
  = None
  | Right
  | Left

data BinOp
  -- | +
  = Add
  -- | *
  | Mul
  -- | -
  | Sub
  -- | /
  | Div
  -- | :
  | Cons
  -- | >>
  | Then
  -- | <<
  | Comp
  -- | &&
  | And
  -- | ||
  | Or
  deriving (Show, Eq)
