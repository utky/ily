-- | Identifiers
module Ily.Syntax.Id where

type Id
  = String

-- Indicator infix as normal ident
data Op
  = Op
  | Nop
  deriving (Show, Eq)

-- Primitives

-- | Identifier qualified with structure id
data Long a
  = Long [StrId] a
  deriving (Eq, Show)

-- 

newtype VId
  = VId Id
  deriving (Eq, Show)

newtype TyVar
  = TyVar Id
  deriving (Eq, Show)

newtype TyCon
  = TyCon Id
  deriving (Eq, Show)

-- | The class Lab is extended to include the numeric labels 1 2 3 ···,
-- i.e. any numeral not starting with 0. 
newtype Lab
  = Lab Id
  deriving (Eq, Show)

newtype StrId
  = StrId Id
  deriving (Eq, Show)
