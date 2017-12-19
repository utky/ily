module Ily.Parser.Expr (exp, dec, decs) where

import           Prelude hiding (exp)
import           Text.Megaparsec hiding (Dec, match)
import           Text.Megaparsec.String (Parser)
import           Text.Megaparsec.Expr
import qualified Ily.Parser.Lexer as L
import qualified Ily.Parser.Id as I
import qualified Ily.Parser.Const as C
import qualified Ily.Parser.Type as T
import qualified Ily.Parser.Pat as P
import           Ily.Parser.Row (rows)
import           Ily.Parser.Seq (seqOf)
import           Ily.Syntax.Id (VId(VId), TyVar)
import           Ily.Syntax.Expr
                   ( AtExp(..)
                   , ExpRow(..)
                   , Exp(..)
                   , Match(..)
                   , MRule(..)
                   , Dec(..)
                   , ValBind(..)
                   , ValRec(..)
                   , TypeBind(..)
                   , DatBind(..)
                   , ConBind(..)
                   , FValBind(..)
                   , FClause(..))

atexp :: Parser AtExp
atexp = choice [ escons
               , evid
               , erec
               , elet
               , elist
               , eselector
               , try etuple -- do not consume if fails for later paren
               , eparen
               ] 
  where
    escons = ESCon <$> C.scon
    evid = EVId <$> I.ope <*> I.longvid
    erec = ERec <$> exprows
    elet = ELet <$> (L.let' *> decs) <*> (L.in' *> exp <* L.end)
    etuple = ETuple <$> seqOf exp
    elist = EList <$> between L.lbracket L.rbracket (sepBy exp L.comma)
    eparen = EParen <$> between L.lparen L.rparen exp
    eselector = ESelector <$> (L.sharp *> I.lab)

exprows :: Parser [ExpRow]
exprows = rows exprow
  where
    exprow = ERow <$> (I.lab <* L.eq) <*> exp

appexp :: Parser Exp
appexp = build <$> some atexp
  where
    build [x] = EAtExp x
    build (x:y:xs)   = foldl consume (EApp x y) xs
    consume e = EApp (EParen e)

exp :: Parser Exp
exp = makeExprParser exp' optable
  where
    optable =
      [ [ eandalso
        ]
      , [ eorelse
        ]
      , [ etyped ]
      ]
    etyped = Postfix $ flip ETyped <$> (L.colon *> T.ty)
    eandalso = InfixR $ EAndAlso <$ L.andalso
    eorelse = InfixR $ EOrElse <$ L.orelse

exp' :: Parser Exp
exp' = choice [ efn
              , ecaseof
              , eifthenelse
              , infexp
              ]
  where
    infexp = choice [ try infixapp, appexp ]
    infixapp = EInfixApp <$> atexp <*> (VId [] <$> L.operator) <*> atexp
    efn = EFn <$> (L.fn *> match)
    ecaseof = ECaseOf <$> (L.case' *> exp) <*> (L.of' *> match)
    eifthenelse = EIf <$> (L.if' *> exp) <*> (L.then' *> exp) <*> (L.else' *> exp)

match :: Parser Match
match = MMRule <$> sepBy1 mrule L.bar

mrule :: Parser MRule
mrule = MRule <$> P.pat <*> (L.fatArrow *> exp)

-- Decl

decs :: Parser [Dec]
decs = (:) <$> dec <*> option [] decs

dec :: Parser Dec
dec = choice [ dval
             , dtype
             , ddatatype
             , dfun
             ] <* optional L.semicolon
  where
    dval  = DVal <$> (L.val *> tyvarseq) <*> valbinds
    dtype = DType <$> (L.type' *> typebinds)
    ddatatype = DDataType <$> (L.datatype *> datbinds)
    dfun = DFun <$> (L.fun *> tyvarseq) <*> fvalbinds

tyvarseq :: Parser [TyVar]
tyvarseq = choice [ single, seq, none ]
  where
    single = fmap (:[]) I.tyvar
    seq = seqOf I.tyvar
    none = return []

valbinds :: Parser [ValBind]
valbinds = sepBy1 valbind L.and

valbind :: Parser ValBind
valbind = VBind <$> P.pat <*> (L.eq *> exp)

typebinds :: Parser [TypeBind]
typebinds = sepBy1 typebind L.and

typebind :: Parser TypeBind
typebind = TBind <$> tyvarseq <*> I.tycon <*> (L.eq *> T.ty)

datbinds :: Parser [DatBind]
datbinds = sepBy1 datbind L.and

datbind :: Parser DatBind
datbind = DBind <$> tyvarseq <*> I.tycon <*> (L.eq *> conbinds)

conbinds :: Parser [ConBind]
conbinds = sepBy1 conbind L.bar

conbind :: Parser ConBind
conbind = CBind <$> I.ope <*> I.vid <*> optional (L.of' *> T.ty)

fvalbinds :: Parser [FValBind]
fvalbinds = sepBy1 fvalbind L.and

fvalbind :: Parser FValBind
fvalbind = FValBind <$> sepBy1 fclause L.bar

fclause :: Parser FClause
fclause = FClause 
            <$> I.ope
            <*> I.vid 
            <*> some P.atpat
            <*> optional (L.colon *> T.ty)
            <*> (L.eq *> exp)
