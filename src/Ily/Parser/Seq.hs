module Ily.Parser.Seq where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import qualified Ily.Parser.Lexer as L

seqOf :: Parser a -> Parser [a]
seqOf p = between L.lparen L.rparen moreThanOne
  where
    moreThanOne = (:) <$> (p <* L.comma) <*> sepBy1 p L.comma
