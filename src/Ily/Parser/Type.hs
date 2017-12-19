module Ily.Parser.Type (ty) where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import           Text.Megaparsec.Expr
import qualified Ily.Parser.Lexer as L
import qualified Ily.Parser.Id as I
import           Ily.Parser.Row (rows)
import           Ily.Syntax.Id (TyVar(..), TyCon(..))
import           Ily.Syntax.Type (Ty(..), TyRow(..))

ty :: Parser Ty
ty = makeExprParser atty optable
  where
    optable =   [ [tfunc] , [ttuple], [ttycon] ] 
    tfunc = InfixR $ TFunc <$ L.arrow
    ttuple = InfixR $ merge <$ L.symbol "*"
    ttycon = Postfix $ (\c t -> TTyCon [t] c) <$> I.longtycon
    merge :: Ty -> Ty -> Ty
    merge t (TTuple ts) = TTuple $ t:ts
    merge t u           = TTuple [t, u]


tyseq :: Parser [Ty]
tyseq = between L.lparen L.rparen tyseq'
  where
    tyseq' = (:) <$> ty <*> (L.comma *> sepBy1 ty L.comma)

tyseqtycon :: Parser Ty
tyseqtycon = TTyCon <$> tyseq <*> I.longtycon

atty :: Parser Ty
atty = choice [ tyvar, tycon, trec, try tyseqtycon, tparen ]
  where
    tyvar = TTyVar <$> I.tyvar
    tycon = TTyCon [] <$> I.longtycon
    trec = TRec <$> tyrows
    tparen = TParen <$> between L.lparen L.rparen ty

tyrows :: Parser [TyRow]
tyrows = rows tyrow
  where
    tyrow = TyRow <$> (I.lab <* L.colon) <*> ty
