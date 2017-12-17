-- | Defines trasformation from bool expression to case-of
module Ily.Derive.If where

import qualified Ily.Syntax.Const as C
import qualified Ily.Syntax.Id as I
import qualified Ily.Syntax.Expr as E
import qualified Ily.Syntax.Pat as P

-- | if-then-else expression
data If
  = If cond thn els
  deriving (Show, Eq)

-- Transform if-then-else to case-of
ifThenElse :: If -> Case.CaseOf
ifThenElse (If cond thn els) =
  Case.CaseOf cond 
    (E.Match $ E.MMRule
      [ makeBoolRule C.true  thn
      , makeBoolRule C.false els
      ])
  where
    makeBoolRule = E.MRule . P.PAtPat . P.PCon

-- | constat true as expression
etrue :: E.Exp
etrue = E.EScon C.true

-- | constat false as expression
efalse :: E.Exp
efalse = E.EScon C.false

-- | logical disjunction 
-- such as : true orelse false -> true
orelse :: E.Exp -> E.Exp -> If
orelse e1 = If e1 etrue

-- | logical conjunction
-- such as : true orelse false -> false
andalso :: E.Exp -> E.Exp -> If 
andalso e1 e2 = If e1 e2 efalse
