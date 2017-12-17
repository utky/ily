module Ily.Parser.Base where

import           Text.Megaparsec

-- | Specialized
type Parser = Parsec String String
