{
module Ily.Parser.Definition where

import Ily.Lexer (Token(..))
import Ily.Syntax (SCons(..))
}

%name sml
%tokentype { Token }
%error { parseError }

%token
    int { Int $$ }

%%

SCons : int { SInt $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
