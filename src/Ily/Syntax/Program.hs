module Ily.Syntax.Program where

import qualified Ily.Syntax.Id as I
import qualified Ily.Syntax.Expr as E

data Program
  = Program [Open] [E.Dec]
  deriving (Show, Eq)

newtype Open
  = Open [I.StrId]
  deriving (Show, Eq)


