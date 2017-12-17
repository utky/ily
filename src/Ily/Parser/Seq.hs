module Ily.Parser.Seq where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import qualified Ily.Parser.Lexer as L

seqOf :: Parser a -> Parser [a]
seqOf p = option [] (try singleton <|> seq')
  where
    singleton = fmap (: []) p
    seq' = between L.lparen L.rparen (sepBy1 p L.comma)
