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

data VId
  = VId [Id] Id
  deriving (Eq, Show)

newtype TyVar
  = TyVar Id
  deriving (Eq, Show)

data TyCon
  = TyCon [Id] Id
  deriving (Eq, Show)

-- | The class Lab is extended to include the numeric labels 1 2 3 ,
-- i.e. any numeral not starting with 0. 
newtype Lab
  = Lab Id
  deriving (Eq, Show)

data StrId
  = StrId [Id] Id
  deriving (Eq, Show)

data SigId
  = SigId [Id] Id
  deriving (Eq, Show)

data FunId
  = FunId [Id] Id
  deriving (Eq, Show)
