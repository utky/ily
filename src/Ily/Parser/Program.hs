module Ily.Parser.Program (program) where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import           Ily.Syntax.Program (Program(..), Open(..))
import qualified Ily.Parser.Lexer as L
import qualified Ily.Parser.Id as I
import qualified Ily.Parser.Expr as E

program :: Parser Program
program = Program <$> many open <*> option [] E.decs

open :: Parser Open
open = Open <$> (L.open *> some I.longstrid)

