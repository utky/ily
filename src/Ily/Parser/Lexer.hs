module Ily.Parser.Lexer where

import           Text.Megaparsec
import           Text.Megaparsec.String 
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L

spaceConsumer :: Parser ()
spaceConsumer = L.space (skipSome spaceChar) lineComment blockComment
  where
    lineComment = L.skipLineComment "#*"
    blockComment = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integer :: Parser Integer
integer = lexeme L.integer

octal :: Parser Integer
octal = lexeme L.octal

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

string :: Parser String
string = char '"' >> manyTill L.charLiteral (char '"')

strings :: Parser String
strings = char '`' >> manyTill L.charLiteral (char '`')

character :: Parser Char
character = char '#' >> between (char '"') (char '"') anyChar

letter :: Parser Char
letter = letterChar

digit :: Parser Char
digit = digitChar

idchar :: Parser Char
--idchar = letter <|> digit <|> char '_' <|> char '\''
idchar = choice [letter, digit, char '_', char '\'']

identifier :: Parser String
identifier = lexeme . reserve $ (:) <$> letter <*> many idchar

operator :: Parser String
operator = lexeme . reserve $  some $ oneOf "!%&$#+-/:<=>?@~'^|*"

tyvarid :: Parser String
tyvarid = lexeme $ (:) <$> char '\'' <*> identifier

qualifiedid :: Parser ([String], String)
qualifiedid = do
  ids <- sepBy1 identifier dot
  let (i:is) = reverse ids
  return (reverse is, i)

abstype :: Parser String
abstype = symbol "abstype"

and :: Parser String
and = symbol "and" 

andalso :: Parser String
andalso = symbol "andalso"

as :: Parser String
as = symbol "as"

case' :: Parser String
case' = symbol "case"

datatype :: Parser String
datatype = symbol "datatype"

do' :: Parser String
do' = symbol "do"

else' :: Parser String
else' = symbol "else"

end :: Parser String
end = symbol "end"

exception :: Parser String
exception = symbol "exception"

fn :: Parser String
fn = symbol "fn"

fun :: Parser String
fun = symbol "fun"

handle :: Parser String
handle = symbol "handle"

if' :: Parser String
if' = symbol "if"

in' :: Parser String
in' = symbol "in"

infix' :: Parser String
infix' = symbol "infix"

infixr' :: Parser String
infixr' = symbol "infixr"

let' :: Parser String
let' = symbol "let"

local :: Parser String
local = symbol "local"

nonfix :: Parser String
nonfix = symbol "nonfix"

of' :: Parser String
of' = symbol "of"

op :: Parser String
op = symbol "op"

open :: Parser String
open = symbol "open"

orelse :: Parser String
orelse = symbol "orelse"

raise :: Parser String
raise = symbol "raise"

rec :: Parser String
rec = symbol "rec"

then' :: Parser String
then' = symbol "then"

type' :: Parser String
type' = symbol "type"

val :: Parser String
val = symbol "val"

with :: Parser String
with = symbol "with"

withtype :: Parser String
withtype = symbol "withtype"

while :: Parser String
while = symbol "while"

lparen :: Parser String
lparen = symbol "("

rparen :: Parser String
rparen = symbol ")"

lbracket :: Parser String
lbracket = symbol "["

rbracket :: Parser String
rbracket = symbol "]"

lbrace :: Parser String
lbrace = symbol "{"

rbrace :: Parser String
rbrace = symbol "}"

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

semicolon :: Parser String
semicolon = symbol ";"

tridot :: Parser String
tridot = symbol "..."

underscore :: Parser String
underscore = symbol "_"

bar :: Parser String
bar = symbol "|"

eq :: Parser String
eq = symbol "="

arrow :: Parser String
arrow = symbol "->"

fatArrow :: Parser String
fatArrow = symbol "=>"

-- | This is not lexeme
sharp :: Parser Char
sharp = char '#'

-- | This is not lexeme
dot :: Parser Char
dot = char '.'

eqtype :: Parser String
eqtype = symbol "eqtype"

functor :: Parser String
functor = symbol "functor"

include :: Parser String
include = symbol "include"

sharing :: Parser String
sharing = symbol "sharing"

sig :: Parser String
sig = symbol "sig"

signature :: Parser String
signature = symbol "signature"

struct :: Parser String
struct = symbol "struct"

structure :: Parser String
structure = symbol "structure"

where' :: Parser String
where' = symbol "where"

sigdep :: Parser String
sigdep = symbol ":>"

doublecolon :: Parser String
doublecolon = symbol "::"

reserve :: Parser String -> Parser String
reserve p = try $ p >>= check
  where
    check s = if s `elem` reserved
                then fail $ "keyword " ++ show s ++ " cannot be an identifier" 
                else return s

reserved :: [String]
reserved = [ "("
           , ")"
           , "["
           , "]"
           , "{"
           , "}"
           , ")"
           , ":"
           , ";"
           , "..."
           , "_"
           , "|"
           , "="
           , "->"
           , "=>"
           , "abstype"
           , "and"
           , "andalso"
           , "as"
           , "case"
           , "datatype"
           , "do"
           , "else"
           , "end"
           , "exception"
           , "fn"
           , "fun"
           , "handle"
           , "if"
           , "in"
           , "infix"
           , "infixr"
           , "let"
           , "local"
           , "nonfix"
           , "of"
           , "op"
           , "open"
           , "orelse"
           , "raise"
           , "rec"
           , "then"
           , "type"
           , "val"
           , "with"
           , "withtype"
           , "while"
           , ":>"
           , "::"
           ]
