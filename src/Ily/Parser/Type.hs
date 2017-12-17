module Ily.Parser.Type (ty) where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import           Text.Megaparsec.Expr
import qualified Ily.Parser.Lexer as L
import qualified Ily.Parser.Id as I
import           Ily.Parser.Row (rows)
import           Ily.Parser.Seq (seqOf)
import           Ily.Syntax.Id (TyVar(..), TyCon(..))
import           Ily.Syntax.Type (Ty(..), TyRow(..))

ty :: Parser Ty
ty = makeExprParser ty' optable
  where
    tfunc = InfixR $ TFunc <$ L.arrow
    ttuple = InfixR $ TPair <$ L.symbol "*"
    tcon = Postfix $ flip TTyCon <$ I.longtycon
    optable =   [ [tcon], [tfunc] , [ttuple] ] 

ty' :: Parser Ty
ty' = choice [ tyvar, trec , tyseq, tycon, vartycon, tparen ]
  where
    tyvar = TTyVar <$> I.tyvar
    trec = TRec <$> tyrows
    tycon = TTyCon [] <$> I.longtycon
    tyseq = TSeq <$> between L.lparen L.rparen (sepBy1 ty L.comma)
    --vartycon = TTyCon <$> seqOf ty <*> I.longtycon
    tparen = between L.lparen L.rparen ty

tyrows :: Parser [TyRow]
tyrows = rows tyrow
  where
    tyrow = TyRow <$> (I.lab <* L.colon) <*> ty
