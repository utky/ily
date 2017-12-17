module Ily.Syntax.Type where

import qualified Ily.Syntax.Id as Id

-- | Type expressoins
data Ty
  -- | type variable
  = TTyVar Id.TyVar
  -- | record type expression
  | TRec [TyRow]
  -- | type construction
  -- | TTyCon [Ty] Id.TyCon
  | TTyCon Ty Id.TyCon
  -- | function type expression (R)
  | TFunc Ty Ty
  -- | paren enclosing
  | TParen Ty
  -- Deriving form
  -- | conjunction of type
  | TTuple [Ty]
  -- intermediate tuple
  | TPair Ty Ty
  | TSeq [Ty]
  deriving (Eq, Show)

-- | Type expression rows
data TyRow
  -- | type expression row
  = TyRow Id.Lab Ty
  deriving (Eq, Show)


