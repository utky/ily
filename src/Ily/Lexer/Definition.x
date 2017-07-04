{
module Ily.Lexer.Definition where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

sml :-

  $white+               ;
  $digit+               { \s -> Int (read s) }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Int Integer
  deriving (Eq,Show)

scanTokens = alexScanTokens
}
