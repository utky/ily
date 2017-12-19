module Ily.Syntax.Module where

import qualified Ily.Syntax.Const as C
import qualified Ily.Syntax.Id as I
import qualified Ily.Syntax.Pat as P
import qualified Ily.Syntax.Type as T
import qualified Ily.Syntax.Expr as E

-- | atomic expressions
data StrExp
  -- | basic
  = StrBasic StrDec
  -- | structure identifier
  | StrIdent I.StrId
  -- | transparent constraint
  | StrConTrans StrExp SigExp
  -- | opaque constraint
  | StrConOpaq StrExp SigExp
  -- | functor application
  | StrFunctorApp I.FunId StrExp
  -- | local decralation
  | StrLet StrDec StrDec
  deriving (Show, Eq)

data StrDec
  -- | declaration
  = StrDec [E.Dec]
  -- | structure
  | Structure [StrBind]
  -- | local
  | StrLocal StrDec StrDec
  -- | empty
  | StrEmpty
  -- | sequential
  | StrSeq [StrDec]
  deriving (Show, Eq)

data StrBind
  -- | structure bind
  = StrBind I.StrId StrExp
  deriving (Show, Eq)

data SigExp
  -- | basic
  = SigBasic [Spec]
  -- | isgnature identifier
  | SigIdent I.SigId
  -- | type realisation
  | SigType SigExp [I.TyVar] I.TyCon T.Ty
  deriving (Show, Eq)

data SigDec
  = Signature [SigBind]
  deriving (Show, Eq)

data SigBind
  = SigBind I.SigId SigExp
  deriving (Show, Eq)

data Spec
  -- | value
  = SpecVal [ValDesc]
  -- | type
  | SpecType [TypeDesc]
  -- | eqtype
  | SpecEqType [TypeDesc]
  -- | datatype
  | SpecDataType [DataDesc]
  -- | replication
  -- | SpecDataRep 
  -- | exception
  -- | SpecExc ExDesc
  -- | structure
  | SpecStr [StrDesc]
  -- | include
  | SpecInc SigExp
  -- | sequential
  -- | SpecSeq [Spec]
  -- | sharing
  -- | SpecShare Spec 
  deriving (Show, Eq)

data ValDesc
  = ValDesc I.VId T.Ty
  deriving (Show, Eq)

data TypeDesc
  = TypeDesc [I.TyVar] I.TyCon
  deriving (Show, Eq)

data DataDesc
  = DataDesc [I.TyVar] I.TyCon [ConDesc]
  deriving (Show, Eq)

data ConDesc
  = ConDesc I.VId (Maybe T.Ty)
  deriving (Show, Eq)

-- data ExDesc

data StrDesc
  = StrDesc I.StrId SigExp
  deriving (Show, Eq)

data FunDec
  = Functor [FunBind]
  deriving (Show, Eq)

data FunBind
  = FunBind I.FunId I.StrId SigExp StrExp
  deriving (Show, Eq)

data TopDec
  -- | structure-level declaration
  = TopStr StrDec
  -- | signature declaration
  | TopSig SigDec
  -- | functor declaration
  | TopFun FunDec
  deriving (Show, Eq)

