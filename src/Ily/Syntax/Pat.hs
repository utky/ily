module Ily.Syntax.Pat where

import qualified Ily.Syntax.Const as Const
import qualified Ily.Syntax.Type as T
import qualified Ily.Syntax.Id as Id

-- | Atomic patterns
data AtPat
  -- | wildcard
  = PWildcard
  -- | special constant
  | PCon Const.SCons
  -- | value identifier
  | PVId Id.Op Id.VId
  -- | record
  | PRec [PatRow]
  -- | parens enclosing
  | PParen Pat
  -- | tuple
  | PTuple [Pat]
  deriving (Eq, Show)

-- | Pattern rows
data PatRow
  -- | wildcard
  = PRWildcard
  -- | pattern row
  -- | PRow Id.Lab Pat [PatRow]
  | PRow Id.Lab Pat
  deriving (Eq, Show)

-- | Patterns
data Pat
  -- | atomic
  = PAtPat AtPat
  -- | flatten deconstruction (intermediate structure)
  | PFlatApp [AtPat]
  -- | constructed pattern
  | PCtor Id.Op Id.VId AtPat
  -- | infixed value construction
  | PInfix Pat Id.VId Pat
  -- | typed
  | PTyped Pat T.Ty
  -- | layered
  | PLayer Id.Op Id.VId (Maybe T.Ty) Pat
  deriving (Eq, Show)

