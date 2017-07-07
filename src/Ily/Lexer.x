{
module Ily.Lexer
  ( Token(..)
  , P
  , lexer
  , parseFail
  , run
  , scanner
  ) where
}

%wrapper "monad"

-- Integer classes
$digit       = 0-9
$octit       = 0-7
$hexit       = [0-9 A-F a-f]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+

-- Character classes
$nl          = [\n\r]
$large       = [A-Z \xc0-\xd6 \xd8-xde]
$small       = [a-z \xdf-\xf6 \xf8-xff]
$alpha       = [a-zA-Z]
$graphic     = [$small $large $digit]

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $white+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap | $nl

sml :-

<0>  $white+               ;
<0>  $digit+               { action (TInt . read) }
<0>  \"                    { begin string }
<string> @string*          { action TStr }
<string> \"                { begin 0 }

{

-- The token type:
data Token
  = TInt Integer
  | TStr String
  | TEOF
  deriving (Eq,Show)

action :: (String -> Token) -> AlexAction Token
action f = token handler where
  handler (pos, prev, rest, input) len = f (take len input)

alexEOF = return TEOF

type P = Alex

lexer :: (Token -> P a) -> P a
lexer f = alexMonadScan >>= f

parseFail = alexError

scanner = alexMonadScan

run = flip runAlex
}
