module Ily.Syntax.Type where

import qualified Ily.Syntax.Id as Id

-- | Type expressoins
data Ty
  -- | type variable
  = TyTyVar Id.TyVar
  -- | record type expression
  | TyRec [TyRow]
  -- | type construction
  | TyTyCon [Ty] Id.TyCon
  -- | function type expression (R)
  | TyFunc Ty Ty
  -- | paren enclosing
  | TyParen Ty
  -- Deriving form
  -- | conjunction of type
  | TyTuple [Ty]
  deriving (Eq, Show)

-- | Type expression rows
data TyRow
  -- | type expression row
  = TyRow Id.Lab Ty
  deriving (Eq, Show)


