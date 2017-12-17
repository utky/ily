module Ily.Parser.Row where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import qualified Ily.Parser.Lexer as L

rows :: Parser a -> Parser [a]
rows p = between L.lbrace L.rbrace (sepBy p L.comma)
