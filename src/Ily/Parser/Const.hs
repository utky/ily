module Ily.Parser.Const where

import           Text.Megaparsec (choice)
import           Text.Megaparsec.String (Parser)
import qualified Ily.Parser.Lexer as L
import           Ily.Syntax.Const (SCons(..))

scon :: Parser SCons
scon = choice [int, str, strs, chr]
  where
    int  = SInt <$> L.integer
    str  = SStr <$> L.string
    strs = SStr <$> L.strings
    chr  = SChar <$> L.character
