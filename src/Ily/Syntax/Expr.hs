module Ily.Syntax.Expr where

import           Prelude hiding (exp)
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
  | ELet [Dec] Exp
  -- | paren enclosing
  | EParen Exp
  -- Deriving forms
  -- Tuple construction literal
  | ETuple [Exp]
  -- List construction literal
  | EList  [Exp]
  -- Record selector with label
  | ESelector I.Lab
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
  | EApp AtExp AtExp
  -- | infixed application
  | EInfixApp AtExp I.VId AtExp
  -- | typed (L)
  | ETyped Exp T.Ty
  -- | handle exception
  -- | TODO
  -- | EHandle Exp Match
  -- | raise exception
  -- | TODO
  -- | ERaise Exp
  -- | function
  | EFn Match
  -- Following constructors belong to derived form
  -- | case of
  | ECaseOf Exp Match
  -- | if then else
  | EIf Exp Exp Exp
  -- | andalso
  | EAndAlso Exp Exp
  -- | orelse
  | EOrElse Exp Exp
  deriving (Show, Eq)

-- | Matches
newtype Match
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
  -- | abstype declaration (this may not be used)
  | DAbsType [DatBind] Dec
  -- | exception declaration
  | DExc [ExBind]
  -- | local declaration
  | DLocal Dec Dec
  -- | open declaration (N >= 1)
  | DOpen [I.StrId]
  -- | infix (L) directive
  | DInfix  (Maybe Integer) [I.VId]
  -- | infix (R) directive
  | DInfixr (Maybe Integer) [I.VId]
  -- | nonfix directive
  | DNonfix [I.VId]
  -- Driving form
  -- | function declaration
  | DFun [I.TyVar] [FValBind]
  deriving (Show, Eq)

-- | Value bindings
data ValBind
  -- | Value bind of reursion, pattern to be bound, expression
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

-- Deriving function which manipulate AST tree
-- =============================================

newtype FValBind
  = FValBind [FClause]
  deriving (Show, Eq)

data FClause
  = FClause I.Op I.VId [P.AtPat] (Maybe T.Ty) Exp
  deriving (Show, Eq)

-- | Derive case-of to anonymous function application
caseOf :: Exp -> Exp
caseOf (ECaseOf e m) = EApp (EParen (EFn m)) (EParen e)
-- fallback to no derivation
caseOf exp           = exp
