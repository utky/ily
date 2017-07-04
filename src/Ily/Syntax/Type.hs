module Ily.Syntax.Type where

import qualified Ily.Syntax.Id as Id

-- | Type expressoins
data Ty
  -- | type variable
  = TTyVar Id.TyVar
  -- | record type expression
  | TRec [TyRow]
  -- | type construction
  | TTyCon [Ty] (Id.Long Id.TyCon)
  -- | function type expression (R)
  | TFunc Ty Ty
  -- | paren enclosing
  | TyParen Ty
  deriving (Eq, Show)

-- | Type expression rows
data TyRow
  -- | type expression row
  = TRow Id.Lab Ty
  deriving (Eq, Show)


