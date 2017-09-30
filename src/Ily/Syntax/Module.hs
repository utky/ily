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
  | StrTransConstr StrExp SigExp
  -- | opaque constraint
  | StrOpaqConstr StrExp SigExp
  -- | functor application
  | StrFunctorApp I.FunId StrExp
  -- | local decralation
  | StrLet StrDec StrDec
  deriving (Show, Eq)

data StrDec
  -- | declaration
  = StrDec E.Dec
  -- | structure
  | Structure [StrBind]
  -- | local
  | StrLocal StrDec StrDec
  -- | empty
  | StrEmpty
  -- | sequential
  | StrSeq [StrDec]

data StrBind
  -- | structure bind
  = StrBind I.StrId StrExp


data SigExp
