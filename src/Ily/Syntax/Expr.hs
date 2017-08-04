module Ily.Syntax.Expr where

import qualified Ily.Syntax.Const as C
import qualified Ily.Syntax.Id as I
import qualified Ily.Syntax.Pat as P
import qualified Ily.Syntax.Type as T

-- | atomic expressions
data AtExp
  -- | special constant
  = ESCon C.SCons
  -- | value identifier
  | EVId I.Op I.VId
  -- | record
  | ERec [ExpRow]
  -- | local declaration
  | ELet Dec Exp
  -- | paren enclosing
  | EParen Exp
  deriving (Show, Eq)

-- | Expression rows
data ExpRow
  -- | expression row
  = ERow I.Lab Exp
  deriving (Show, Eq)

-- | Expressions
data Exp
  -- | atomic
  = EAtExp AtExp
  -- | application (L)
  | EApp Exp AtExp
  -- | infixed application
  | EInfixApp Exp I.VId Exp
  -- | typed (L)
  | ETyped Exp T.Ty
  -- | handle exception
  | EHandle Exp Match
  -- | raise exception
  | ERaise Exp
  -- | function
  | EFn Match
  deriving (Show, Eq)

-- | Matches
data Match
  = MMRule [MRule]
  deriving (Show, Eq)
  -- recursive (need optimise)

-- | Match rules
data MRule
  = MRule P.Pat Exp
  deriving (Show, Eq)


-- | Declarations
data Dec
  -- | value declaration
  = DVal [I.TyVar] [ValBind]
  -- | type declaration
  | DType [TypeBind]
  -- | datatype declaration
  | DDataType [DatBind]
  -- | datatype replication
  | DDataTypeRep I.TyCon I.TyCon -- ???
  -- | abstype declaration
  | DAbsType [DatBind] Dec
  -- | exception declaration
  | DExc [ExBind]
  -- | local declaration
  | DLocal Dec Dec
  -- | open declaration (N >= 1)
  | DOpen [I.StrId]
  -- | sequantial declaration
  | DSeq Dec Dec
  -- | infix (L) directive
  | DInfix  (Maybe Integer) [I.VId]
  -- | infix (R) directive
  | DInfixr (Maybe Integer) [I.VId]
  -- | nonfix directive
  | DNonfix [I.VId]
  deriving (Show, Eq)

-- | Value bindings
data ValBind
  = VBind P.Pat Exp
  deriving (Show, Eq)

data ValRec
  = ValRec
  deriving (Show, Eq)

-- | Type bindings
data TypeBind
  = TBind [I.TyVar] I.TyCon T.Ty
  deriving (Show, Eq)

-- | Datatype bindings
data DatBind
  = DBind [I.TyVar] I.TyCon [ConBind]
  deriving (Show, Eq)

-- | Constructor bindings
data ConBind
  = CBind I.Op I.VId (Maybe T.Ty)
  deriving (Show, Eq)

-- | Exception bindings
data ExBind
  = ExBind I.Op I.VId (Maybe T.Ty)
  deriving (Show, Eq)



