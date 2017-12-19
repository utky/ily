module Ily.Parser.Pat (pat) where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import           Text.Megaparsec.Expr
import qualified Ily.Parser.Lexer as L
import qualified Ily.Parser.Id as I
import qualified Ily.Parser.Const as C
import qualified Ily.Parser.Type as T
import           Ily.Parser.Row (rows)
import           Ily.Parser.Seq (seqOf)
import           Ily.Syntax.Id (VId(VId))
import           Ily.Syntax.Pat (AtPat(..), PatRow(..), Pat(..))

atpat :: Parser AtPat
atpat = choice [ pwildcard
               , pscons
               , pvid
               , prec
               , try ptuple -- do not consume if fails for later paren
               , try pparen
               , punit
               ] 
  where
    pwildcard = PWildcard <$ L.underscore
    pscons = PCon <$> C.scon
    pvid = PVId <$> I.ope <*> I.longvid
    prec = PRec <$> patrows
    ptuple = PTuple <$> seqOf pat
    pparen = PParen <$> between L.lparen L.rparen pat
    punit  = PUnit <$ L.lparen <* L.rparen

patrows :: Parser [PatRow]
patrows = rows (choice [ pwildcard, patrow ])
  where
    pwildcard = PRWildcard <$ L.tridot
    patrow = PRow <$> (I.lab <* L.eq) <*> pat

pat :: Parser Pat
pat = makeExprParser pat' optable
  where
    optable = [ [ ptyped ] ]
    ptyped = Postfix $ flip PTyped <$> (L.colon *> T.ty)
    -- pcons = InfixR $ (\s x y -> PInfix x (VId [] s) y) <$> L.symbol "::"

pat' :: Parser Pat
pat' = choice [ try pinfix
              , try pctor
              , patpat
              ]
  where
    patpat = PAtPat <$> atpat
    pinfix = PInfix <$> atpat <*> I.vid <*> atpat
    pctor = PCtor <$> I.ope <*> I.longvid <*> atpat
