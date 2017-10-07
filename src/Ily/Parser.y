{
{-# OPTIOINS_GHC -a -g -c #-}
module Ily.Parser where

import Ily.Lexer (Token(..), P, lexer, parseFail)
import Ily.Syntax
  ( SCons(..)
  , Id
  , VId(..)
  , TyVar(..)
  , TyCon(..)
  , Lab(..)
  , StrId(..)
  , SigId(..)
  , FunId(..)
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
  , StrExp(..)
  , StrDec(..)
  , StrBind(..)
  , SigExp(..)
  , SigDec(..)
  , SigBind(..)
  , Spec(..)
  , ValDesc(..)
  , TypeDesc(..)
  , DataDesc(..)
  , ConDesc(..)
  , StrDesc(..)
  , FunDec(..)
  , FunBind(..)
  , TopDec(..)
  , Program(..)
  )
}

%monad{P}
%lexer{lexer}{TEOF}
%name parseScons scon
%name parsePat   pat
%name parseType   ty
%name parseExp   exp
%name parseDec   dec
%name parseTop   topdec
%name parseProg  program
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
    -- eqtype
    eqtype { TEqType }
    -- functor
    functor { TFunctor }
    -- include
    include { TInclude }
    -- sharing
    sharing { TSharing }
    -- sig
    sig     { TSig }
    -- signature
    signature { TSigunature }
    -- struct
    struct    { TStruct }
    -- structure
    structure { TStructure }
    -- where
    where     { TWhere }
    -- :>
    ':>'      { TSigDep }
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

%nonassoc ':'
%right    ','
%right    '->'
%right    '=>'
%right    and
%left     else
%left     raise
%right    handle
%right    orelse
%right    andalso
%right    as

%%

-- Combinators

option(p)  :    { Nothing }
           | p  { Just $1 }
           

fst(p,q)        : p q                 { $1 }
snd(p,q)        : p q                 { $2 }
both(p,q)       : p q                 { ($1,$2) }

-- rev_seq1(p)  : p              { [$1] }
--              | rev_seq1(p) p { $2 : $1 }
-- 
-- 
-- seq1(p)        : rev_seq1(p)        { reverse $1 }
-- seq(p)         : seq1(p)            { $1 }
--                |                    { [] }
-- 
rev_sep(p, q)  :                         { [] }
               | p                       { [$1] }
               | rev_sep(p, q) snd(q, p) { $2 : $1 }
               
-- 
-- rev_sep1(p, q)  : rev_sep1(p, q) snd(q, p) { $2 : $1 }
--                 | p                     { [$1] }
-- 
sep(p, q)      : rev_sep(p, q)          { reverse $1 }
-- sep1(p, q)      : rev_sep1(p, q)    { reverse $1 }
-- 
-- rows(p)        : sep(p, ',')         { $1 }
-- 
-- andseq(p)      : sep1(p, and)        { $1 }

seq(p)         :                        { [] }
               | p                      { [$1] }
               | '(' sep(p, ',') ')'    { $2 }

list_(p)       :             { [] }
               | list_(p) p  { $2 : $1 }
               

list_1(p)      : p            { [$1] }
               | list_1(p) p  { $2 : $1 }
               

list(p)        : list_(p)    { reverse $1 }

list1(p)       : list_1(p)   { reverse $1 }

-- | Special constant
scon :: { SCons }
      : int    { SInt $1 }
      | string { SStr $1 }
      | char   { SChar $1 }

-- | Identifier TODO

vid :: { VId }
    : id   { VId $1 }

longvid :: { VId }
        : id     { VId $1 }
        | longid { makeLong QVId $1 }

tyvar :: { TyVar }
      : tyvarid   { TyVar $1 }

tycon :: { TyCon }
      : id   { TyCon $1 }

longtycon :: { TyCon }
          : id     { TyCon $1 }
          | longid { makeLong QTyCon $1 }

strid     :: { StrId }
          : id   { StrId $1 }

longstrid :: { StrId }
          : id     { StrId $1 }
          | longid { makeLong QStrId $1 }

sigid     :: { SigId }
          : id     { SigId $1 }
          | longid { makeLong QSigId $1 }

funid     :: { FunId }
          : id     { FunId $1 }
          | longid { makeLong QFunId $1 }

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
      | '{' patrows '}' { PRec $2 }
      | '(' ')'  { PTuple [] }
      | '(' pat ')'          { PPat $2 }
      | '(' sep(pat, ',') ')'  { PTuple $2 }
      -- ^ TODO: tuple element must be > 1 
      --         test

patrows :: { [PatRow] }
        : patrows_ { reverse $1 }

patrows_ :: { [PatRow] }
         :                     { [] }
         | patrow              { [$1] }
         | patrows_ ',' patrow { $3 : $1 }

patrow :: { PatRow }
       :  '...'       { PRWildcard }
       | lab '=' pat  { PRow $1 $3 }

apppat  :: { [AtPat] }
        : apppat_       { reverse $1 }

apppat_ :: { [AtPat] }
        : atpat         { [$1] }
        | apppat_ atpat { $2 : $1 }

pat :: { Pat }
    : apppat                              { PFlatApp $1 }
    -- : atpat                               { PAtPat $1 }
    -- | ope longvid atpat                   { PCtor $1 $2 $3 }
    -- ^ FIXME: this pattern is prevented by PInfix patten
    -- parser seems to choose infix pattern for "just x" as 
    -- "just x SOMETHING"
    -- | pat vid pat                         { PInfix $1 $2 $3 }
    --TODO: ^ has s/r and r/r conflict
    -- not neccesary needed -> pending impl
    | pat ':' ty                          { PTyped $1 $3 }
    --| ope vid option(snd(':', ty)) as pat { PLayer $1 $2 $3 $5 }

ty :: { Ty }
   : tyvar                { TyTyVar $1 }
   | '{' tyrows '}'       { TyRec $2 }
   | ty '->' ty           { TyFunc $1 $3 }
   | tytycon              { $1 }
   | '(' ty ')'           { TyParen $2 }

tytycon :: { Ty }
        : longtycon              { TyTyCon [] $1 }
        | ty longtycon           { TyTyCon [$1] $2 }
        | seq(ty) longtycon      { TyTyCon $1 $2 }

tyrows :: { [TyRow] }
       : tyrows_ { reverse $1 }

tyrows_ :: { [TyRow] }
        :                   { [] }
        | tyrow             { [$1] }
        | tyrows_ ',' tyrow { $3 : $1 }


tyrow :: { TyRow }
      : lab ':' ty { TyRow $1 $3 }

-- ========================================================
-- | Expression
-- ========================================================
atexp :: { AtExp }
      : scon                 { ESCon $1 }
      | ope longvid          { EVId $1 $2 }
      | '{' exprows '}'      { ERec $2 }
      | let dec in exp end   { ELet $2 $4 }
      -- ^ TODO: declaration
      | '(' exp ')'          { EParen $2 }

exprows :: { [ExpRow] }
       : exprows_ { reverse $1 }

exprows_ :: { [ExpRow] }
        :                   { [] }
        | exprow             { [$1] }
        | exprows_ ',' exprow { $3 : $1 }

exprow :: { ExpRow }
       : lab '=' exp { ERow $1 $3 }

appexp  :: { [AtExp] }
        : appexp_      { reverse $1 }

appexp_ :: { [AtExp] }
        : atexp        { [$1] }
        | appexp_ atexp { $2 : $1 }

-- infexp :: { Exp }
--        -- : infexp vid infexp { EInfixApp $1 $2 $3 }
--        : atexp vid atexp { EInfixApp $1 $2 $3 }
--        | appexp { $1 }

exp :: { Exp }
    -- : infexp { $1 }
    : appexp      { EFlatApp $1 }
    | exp ':' ty  { ETyped $1 $3 }
    -- | exp handle match { EHandle $1 $3 }
    -- ^ TODO: not used
    -- | raise exp { ERaise $2 }
    -- ^ TODO: not used
    | fn match { EFn $2 }

match :: { Match }
      : mrules { MMRule $1 }

mrules :: { [MRule] }
       : mrules_ { reverse $1 }

mrules_ :: { [MRule] }
        : mrule             { [$1] }
        | mrules_ '|' mrule { $3 : $1 }

mrule :: { MRule }
      : pat '=>' exp  { MRule $1 $3 }

dec :: { Dec }
    : dec_ option(';')  { $1 }

-- | Declaration
dec_ :: { Dec }
    : val seq(tyvar) valbinds       { DVal $2 $3 }
    | val valbinds                  { DVal [] $2 }
    | type typbinds                 { DType $2 }
    | datatype datbinds             { DDataType $2 }
    -- TODO Data type rep concrete syntax
    -- abstype and exception is not mandatory
--    | abstype datbinds with dec end { DAbsType $2 $4 }
--    | exception exbinds             { DExc $2 }
    | local dec in dec end           { DLocal $2 $4 }
    | open list1(longstrid)          { DOpen $2 }
    | decs                           { DSeq $1 }
    -- ^ TODO: this make 7 r/r conflict
    | infix option(int) list1(vid)   { DInfix $2 $3 }
    | infixr option(int) list1(vid)  { DInfixr $2 $3 }
    | nonfix list1(vid) { DNonfix $2 }

decs :: { [Dec] }
     : decs_ { reverse $1 }

decs_ :: { [Dec] }
      : dec       { [$1] }
      | decs_ dec { $2 : $1 }

-- | TODO Recursive
valbinds :: { [ValBind] }
         : sep(valbind, and) { $1 }

valbind  :: { ValBind }
         : pat '=' exp  { VBind $1 $3 }

typbinds :: { [TypeBind] }
         : sep(typbind, and) { $1 }

typbind :: { TypeBind }
        : seq(tyvar) tycon '=' ty { TBind $1 $2 $4 }
        | tycon '=' ty { TBind [] $1 $3 }

datbinds :: { [DatBind] }
         : sep(datbind, and) { $1 }

datbind :: { DatBind }
        : seq(tyvar) tycon '=' conbinds { DBind $1 $2 $4 }
        | tycon '=' conbinds { DBind [] $1 $3 }

conbinds :: { [ConBind] }
         : sep(conbind, '|')  { $1 }

conbind :: { ConBind }
        : ope vid option(snd(of, ty)) { CBind $1 $2 $3 }

-- TODO: No longer use exception for sake of simplicity of syntax
-- exbinds :: { [ExBind] }
--         : sep(exbind, and) { $1 }
-- 
-- exbind :: { ExBind }
--        : ope vid option(snd(of, ty)) { ExBind $1 $2 $3 }

-- ========================================================
-- | Module
-- ========================================================
strexp :: { StrExp }
       : struct strdec end        { StrBasic $2 }
       | longstrid                { StrIdent $1 }
       | strexp ':'  sigexp       { StrConTrans $1 $3 }
       | strexp ':>' sigexp       { StrConOpaq $1 $3 }
       | funid '(' strexp ')'     { StrFunctorApp $1 $3 }
       | let strdec in strdec end { StrLet $2 $4 }

strdec :: { StrDec }
       : dec                    { StrDec $1 }
       | structure strbinds     { Structure $2 }
       | local strdec in strdec { StrLocal $2 $4 }
       -- emtpy
       -- sequential

strbinds :: { [StrBind] }
         : sep(strbind, and) { $1 }

strbind :: { StrBind }
        : strid '=' strexp { StrBind $1 $3 }

sigexp :: { SigExp }
       : sig specs end { SigBasic $2 }
       | sigid         { SigIdent $1 }
       | sigexp where type seq(tyvar) longtycon '=' ty
           { SigType $1 $4 $5 $7 }

sigdec :: { SigDec }
       : signature sigbinds { Signature $2 }

sigbinds :: { [SigBind] }
         : sep(sigbind, and) { $1 }

sigbind :: { SigBind }
        : sigid '=' sigexp { SigBind $1 $3 }

spec :: { Spec }
     : spec_ option(';')   { $1 }

spec_ :: { Spec }
      : val valdescs       { SpecVal $2 }
      | type typedescs     { SpecType $2 }
      | eqtype typedescs   { SpecEqType $2 }
      | datatype datadescs { SpecDataType $2 }
      -- | data rep
      | structure strdescs { SpecStr $2 }
      | include sigexp     { SpecInc $2 }
      -- sharing

specs :: { [Spec] }
     : specs_ { reverse $1 }

specs_ :: { [Spec] }
      : spec       { [$1] }
      | specs_ spec { $2 : $1 }

valdesc :: { ValDesc }
        : vid ':' ty { ValDesc $1 $3 }

valdescs :: { [ValDesc] }
         : sep(valdesc, and) { $1 }

typedesc :: { TypeDesc }
         : seq(tyvar) tycon { TypeDesc $1 $2 }

typedescs :: { [TypeDesc] }
          : sep(typedesc, and) { $1 }

datadesc :: { DataDesc }
         : seq(tyvar) tycon '=' condescs { DataDesc $1 $2 $4 }

datadescs :: { [DataDesc] }
         : sep(datadesc, and) { $1 }

condesc :: { ConDesc }
        : vid option(snd(of, ty)) { ConDesc $1 $2 }

condescs :: { [ConDesc] }
         : sep(condesc, '|')  { $1 }

strdesc :: { StrDesc }
        : strid ':' sigexp { StrDesc $1 $3 }

strdescs :: { [StrDesc] }
         : sep(strdesc, and) { $1 }

fundec :: { FunDec }
       : functor funbinds { Functor $2 }

funbind :: { FunBind }
        : funid '(' strid ':' sigexp ')' '=' strexp { FunBind $1 $3 $5 $8 }

funbinds :: { [FunBind] }
         : sep(funbind, and) { $1 }

topdec :: { TopDec }
       : strdec { TopStr $1 }
       | sigdec { TopSig $1 }
       | fundec { TopFun $1 }

topdecs  :: { [TopDec] }
         : topdecs_ { reverse $1 }

topdecs_ :: { [TopDec] }
         :                 { [] }
         | topdec          { [$1] }
         | topdecs_ topdec { $2 : $1 }

program :: { Program }
        : topdecs { Program $1 }

{
makeLong :: ([String] -> String -> a) -> [String] -> a
makeLong f xs = f (init xs) (last xs)

parseError p = parseFail ("Parse failure: " ++ (show p))
}
