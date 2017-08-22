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
$alpha       = [$small $large]
$unichar     = \x00C0-\xFFFF
$letter      = [$alpha $unichar]
-- TODO: accept more sequence
$graphic     = [$letter $digit]

$symbol = [\!\%\&\$\#\+\-\/\:\<\=\>\?\@\\\~\‘\^\|\*]

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $white+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap | $nl

--- Reserved

@reservedOp = "(" | ")" | "[" | "]" | "{" | "}" | "," | ":" | ";" | "..." 
            | "_" | "=" | "=>" | "->" | "#"

-- Identifies

$symbol = [\!\%\&\$\#\+\-\/\:\<\=\>\?\@\\\~\'\^\|\*]
$idchar      = [$letter $digit \_\']

@id        = ($letter $idchar*) | $symbol+
@tyvarid   = \' $letter $idchar*
@strid     = $letter $idchar*
@longid    = (@strid \.)+ @id

sml :-

<0>  $white+               ;

-- Reserved
<0>  "("                     { action $ const TLParen }
<0>  ")"                     { action $ const TRParen }
<0>  "["                     { action $ const TLBracket }
<0>  "]"                     { action $ const TRBracket }
<0>  "{"                     { action $ const TLBrace }
<0>  "}"                     { action $ const TRBrace }
<0>  ","                     { action $ const TComma }
<0>  ":"                     { action $ const TColon }
<0>  ";"                     { action $ const TSemiColon }
<0>  "..."                   { action $ const TRowWild }
<0>  "_"                     { action $ const TValWild }
<0>  "|"                     { action $ const TBar }
<0>  "="                     { action $ const TEq }
<0>  "=>"                    { action $ const TFatArrow }
<0>  "->"                    { action $ const TArrow }
<0>  "#"                     { action $ const TSharp }
<0>  "abstype"               { action $ const TAbsType }
<0>  "and"                   { action $ const TAnd }
<0>  "andalso"               { action $ const TAndAlso }
<0>  "as"                    { action $ const TAs }
<0>  "case"                  { action $ const TCase }
<0>  "datatype"              { action $ const TDataType }
<0>  "do"                    { action $ const TDo }
<0>  "else"                  { action $ const TElse }
<0>  "end"                   { action $ const TEnd }
<0>  "exception"             { action $ const TException }
<0>  "fn"                    { action $ const TFn }
<0>  "fun"                   { action $ const TFun }
<0>  "handle"                { action $ const THandle }
<0>  "if"                    { action $ const TIf }
<0>  "in"                    { action $ const TIn }
<0>  "infix"                 { action $ const TInfix }
<0>  "infixr"                { action $ const TInfixr }
<0>  "let"                   { action $ const TLet }
<0>  "local"                 { action $ const TLocal }
<0>  "nonfix"                { action $ const TNonfix }
<0>  "of"                    { action $ const TOf }
<0>  "op"                    { action $ const TOp }
<0>  "open"                  { action $ const TOpen } 
<0>  "orelse"                { action $ const TOrElse }
<0>  "raise"                 { action $ const TRaise} 
<0>  "rec"                   { action $ const TRec }
<0>  "then"                  { action $ const TThen }
<0>  "type"                  { action $ const TType }
<0>  "val"                   { action $ const TVal }
<0>  "with"                  { action $ const TWith }
<0>  "withtype"              { action $ const TWithType }
<0>  "while"                 { action $ const TWhile }
<0>  "."                     { action $ const TDot }

<0>  $digit+               { action (TInt . read) }

<0>     \# \"              { begin char }
<char>  $graphic \"        { action (TChar . head) }
<char>  \"                 { begin 0 }

<0>  \"                    { begin string }
<string> @string*          { action TStr }
<string> \"                { begin 0 }

<0>  @id                   { action TId }
<0>  @longid               { action (TLongId . split '.')}
<0>  @tyvarid              { action TTyV }

{

-- The token type:
data Token
  = TInt Integer
  | TStr String
  | TChar Char
  -- abstype
  | TAbsType
  -- and
  | TAnd
  -- andalso
  | TAndAlso
  -- as
  | TAs
  -- case
  | TCase
  -- datatype
  | TDataType
  -- do
  | TDo
  -- else
  | TElse
  -- end
  | TEnd
  -- exception
  | TException
  -- fn
  | TFn
  -- fun
  | TFun
  -- handle
  | THandle
  -- if
  | TIf
  -- in
  | TIn
  -- infix
  | TInfix
  -- infixr
  | TInfixr
  -- let
  | TLet
  -- local
  | TLocal
  -- nonfix
  | TNonfix
  -- of
  | TOf
  -- op
  | TOp
  -- open
  | TOpen
  -- orelse
  | TOrElse
  -- raise
  | TRaise
  -- rec
  | TRec
  -- then
  | TThen
  -- type
  | TType
  -- val
  | TVal
  -- with
  | TWith
  -- withtype
  | TWithType
  -- while
  | TWhile
  -- (
  | TLParen
  -- )
  | TRParen
  -- [
  | TLBracket
  -- ]
  | TRBracket
  -- {
  | TLBrace
  -- }
  | TRBrace
  -- ,
  | TComma
  -- :
  | TColon
  -- ;
  | TSemiColon
  -- ...
  | TRowWild
  -- _
  | TValWild
  -- |
  | TBar
  -- =
  | TEq
  -- ->
  | TArrow
  -- =>
  | TFatArrow
  -- #
  | TSharp
  -- .
  | TDot
  -- Identifier
  | TId String
  -- Long Identifier
  | TLongId [String]
  -- Type variable
  | TTyV String
  -- End of file
  | TEOF
  deriving (Eq,Show)

type P = Alex

action :: (String -> Token) -> AlexAction Token
action f = token handler
  where
    handler (pos, prev, rest, input) len = f (take len input)

split :: Char -> String -> [String]
split d = reverse . fst . finalise . foldl feed ([], "")
  where
    feed :: ([String], String) -> Char -> ([String], String)
    feed (acc, tok) x
      | x == d    = let newAcc = (done tok):acc
                    in  (newAcc, "")
      | otherwise = (acc, comsume tok x)
    comsume s i = i:s
    done s = reverse s
    finalise (acc, "") = (acc, "")
    finalise (acc, tok) = ((done tok):acc, "")

alexEOF = return TEOF


lexer :: (Token -> P a) -> P a
lexer f = alexMonadScan >>= f

parseFail :: String -> P a
parseFail s = do
  ((AlexPn len linen coln), _, _, _) <- alexGetInput
  let msg = s ++ " at " ++ (show linen) ++ ":" ++ (show coln)
  alexError msg

scanner = alexMonadScan

run = flip runAlex
}
