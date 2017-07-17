{
module Ily.Parser where

import Ily.Lexer (Token(..), P, lexer, parseFail)
import Ily.Syntax
  ( SCons(..)
  , Long(..)
  , VId(..)
  , TyVar(..)
  , TyCon(..)
  , Lab(..)
  , StrId(..)
  , Op(..)
  , AtPat(..)
  , PatRow(..)
  , Pat(..)
  , Ty(..)
  , TyRow(..)
  , AtExp(..)
  , ExpRow(..)
  , Exp(..)
  , Match(..)
  , MRule(..)
  , Dec(..)
  , ValBind(..)
  , TypeBind(..)
  , DatBind(..)
  , ConBind(..)
  , ExBind(..)
  )
}

%monad{P}
%lexer{lexer}{TEOF}
%name parseScons scon
%name parseDec   dec
%tokentype { Token }
%error { parseError }

%token
    int    { TInt $$ }
    string { TStr $$ }
    char   { TChar $$ }
    -- abstype
    abstype  { TAbsType }
      -- and
    and  { TAnd }
      -- andalso
    andalso  { TAndAlso }
      -- as
    as  { TAs }
      -- case
    case  { TCase }
      -- datatype
    datatype  { TDataType }
      -- do
    do  { TDo }
      -- else
    else  { TElse }
      -- end
    end  { TEnd }
      -- exception
    exception  { TException }
      -- fn
    fn  { TFn }
      -- fun
    fun  { TFun }
      -- handle
    handle  { THandle }
      -- if
    if  { TIf }
      -- in
    in  { TIn }
      -- infix
    infix  { TInfix }
      -- infixr
    infixr  { TInfixr }
      -- let
    let  { TLet }
      -- local
    local  { TLocal }
      -- nonfix
    nonfix  { TNonfix }
      -- of
    of  { TOf }
      -- op
    op  { TOp }
      -- open
    open  { TOpen }
      -- orelse
    orelse  { TOrElse }
      -- raise
    raise  { TRaise }
      -- rec
    rec  { TRec }
      -- then
    then  { TThen }
      -- type
    type  { TType }
      -- val
    val  { TVal }
      -- with
    with  { TWith }
      -- withtype
    withtype  { TWithType }
      -- while
    while  { TWhile }
      -- (
    '('  { TLParen }
      -- ) 
    ')'  { TRParen }
      -- [ 
    '['  { TLBracket }
      -- ] 
    ']'  { TRBracket }
      -- { 
    '{'  { TLBrace }
      -- } 
    '}'  { TRBrace }
      -- , 
    ','  { TComma }
      -- : 
    ':'  { TColon }
      -- ; 
    ';'  { TSemiColon }
      -- ... 
    '...'  { TRowWild }
      -- _ 
    '_'  { TValWild }
      -- { 
    '|'  { TBar }
      -- = 
    '='  { TEq }
      -- -> 
    '->'  { TArrow }
      -- => 
    '=>'  { TFatArrow }
      -- # 
    '#'  { TSharp }
      -- # 
    '.'  { TDot }
      -- Identifier 
    id  { TId $$ }
    longid { TLongId $$ }
    tyvarid  { TTyV $$ }

%%

-- Combinators

option(p)  : p  { Just $1 }
           |    { Nothing }

rev_seq1(p)  : p              { [$1] }
             | rev_seq1(p) p { $2 : $1 }

fst(p,q)        : p q                 { $1 }
snd(p,q)        : p q                 { $2 }
both(p,q)       : p q                 { ($1,$2) }

seq1(p)        : rev_seq1(p)        { reverse $1 }
seq(p)         : seq1(p)            { $1 }
               |                    { [] }

rev_sep(p, q)  : rev_sep(p, q) snd(q, p) { $2 : $1 }
               |                     { [] }

rev_sep1(p, q)  : rev_sep1(p, q) snd(q, p) { $2 : $1 }
                | p                     { [$1] }

sep(p, q)       : rev_sep(p, q)     { reverse $1 }
sep1(p, q)      : rev_sep1(p, q)    { reverse $1 }

rows(p)        : sep(p, ',')         { $1 }

andseq(p)      : sep1(p, and)        { $1 }

-- | Special constant
scon :: { SCons }
      : int    { SInt $1 }
      | string { SStr $1 }
      | char   { SChar $1 }

-- | Identifier TODO

vid :: { VId }
    : id   { VId $1 }

longvid :: { Long VId }
        : longid { makeLong VId $1 }

tyvar :: { TyVar }
      : tyvarid   { TyVar $1 }

tycon :: { TyCon }
      : id   { TyCon $1 }

longtycon :: { Long TyCon }
          : longid { makeLong TyCon $1 }

longstrid :: { Long StrId }
          : longid { makeLong StrId $1 }

-- TODO: could be int as label of tuple
lab :: { Lab }
    : id   { Lab $1 }

ope :: { Op }
    :     { Nop }
    | op  { Op }

-- | Pattern
atpat :: { AtPat }
      : '_'                  { PWildcard }
      | scon                 { PCon $1 }
      | ope longvid          { PVId $1 $2 }
      | '{' rows(patrow) '}' { PRec $2 }
      | '(' pat ')'          { PPat $2 }

patrow :: { PatRow }
       : '...'       { PRWildcard }
       | lab '=' pat { PRow $1 $3 }

pat :: { Pat }
    : atpat                               { PAtPat $1 }
    | ope longvid atpat                   { PCtor $1 $2 $3 }
    | pat vid pat                         { PInfix $1 $2 $3 }
    | pat ':' ty                          { PTyped $1 $3 }
    | ope vid option(snd(':', ty)) as pat { PLayer $1 $2 $3 $5 }

ty :: { Ty }
   : tyvar                { TyTyVar $1 }
   | '{' rows(tyrow) '}'  { TyRec $2 }
   | seq(ty) longtycon    { TyTyCon $1 $2 }
   | ty '->' ty           { TyFunc $1 $3 }
   | '(' ty ')'           { TyParen $2 }

tyrow :: { TyRow }
      : lab ':' ty { TyRow $1 $3 }

-- | Expression
atexp :: { AtExp }
      : scon                 { ESCon $1 }
      | ope longvid          { EVId $1 $2 }
      | '{' rows(exprow) '}' { ERec $2 }
      | let dec in exp end   { ELet $2 $4 }
      | '(' exp ')'          { EParen $2 }

exprow :: { ExpRow }
       : lab '=' exp { ERow $1 $3 }

exp :: { Exp }
    : atexp { EAtExp $1 }
    | exp atexp { EApp $1 $2 }
    | exp vid exp { EInfixApp $1 $2 $3 }
    | exp ':' ty  { ETyped $1 $3 }
    | exp handle match { EHandle $1 $3 }
    | raise exp { ERaise $2 }
    | fn match { EFn $2 }

match :: { Match }
      : sep1(mrule, '|') { MMRule $1 }

mrule :: { MRule }
      : pat '=>' exp  { MRule $1 $3 }

-- | Declaration
dec :: { Dec }
    : val seq(tyvar) valbinds { DVal $2 $3 }
    | type typbinds          { DType $2 }
    | datatype datbinds      { DDataType $2 }
    -- TODO Data type rep concrete syntax
    | abstype datbinds with dec end { DAbsType $2 $4 }
    | exception exbinds { DExc $2 }
    | local dec in dec end { DLocal $2 $4 }
    | open seq1(longstrid) { DOpen $2 }
    | dec option(';') dec { DSeq $1 $3 }
    | infix option(int) seq1(vid) { DInfix $2 $3 }
    | infixr option(int) seq1(vid) { DInfixr $2 $3 }
    | nonfix seq1(vid) { DNonfix $2 }

-- | TODO Recursive
valbinds :: { [ValBind] }
         : andseq(valbind) { $1 }

valbind  :: { ValBind }
         : pat '=' exp  { VBind $1 $3 }

typbinds :: { [TypeBind] }
         : andseq(typbind) { $1 }

typbind :: { TypeBind }
        : seq(tyvar) tycon '=' ty { TBind $1 $2 $4 }

datbinds :: { [DatBind] }
         : andseq(datbind) { $1 }

datbind :: { DatBind }
        : seq(tyvar) tycon '=' conbinds { DBind $1 $2 $4 }

conbinds :: { [ConBind] }
         : sep1(conbind, '|')  { $1 }

conbind :: { ConBind }
        : ope vid option(snd(of, ty)) { CBind $1 $2 $3 }

exbinds :: { [ExBind] }
        : andseq(exbind) { $1 }

exbind :: { ExBind }
       : ope vid option(snd(of, ty)) { ExBind $1 $2 $3 }

{
makeLong :: (String -> a) -> [String] -> Long a
makeLong f xs = Long (map StrId (init xs)) (f $ last xs)

parseError _ = parseFail "Parse failure"
}
