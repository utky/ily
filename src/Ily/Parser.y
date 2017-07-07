{
module Ily.Parser where

import Ily.Lexer (Token(..), P, lexer, parseFail)
import Ily.Syntax (SCons(..))
}

%monad{P}
%lexer{lexer}{TEOF}
%name parse
%tokentype { Token }
%error { parseError }

%token
    int { TInt $$ }

%%

SCons : int { SInt $1 }

{
parseError _ = parseFail "Parse failure"
}
