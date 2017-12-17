module Ily.Parser.Id where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import qualified Ily.Parser.Lexer as L
import           Ily.Syntax.Id 
                   ( Id(..)
                   , Op(..)
                   , VId(..)
                   , TyVar(..)
                   , TyCon(..)
                   , Lab(..)
                   , StrId(..)
                   , SigId(..)
                   , FunId(..))

ope :: Parser Op
ope = maybeOp <$> optional L.op
  where
    maybeOp (Just _) = Op
    maybeOp Nothing  = Nop

vid :: Parser VId
vid = VId <$> (L.identifier <|> L.operator)

tyvar :: Parser TyVar
tyvar = TyVar <$> L.tyvarid

tycon :: Parser TyCon
tycon = TyCon <$> (L.identifier <|> L.operator)

lab :: Parser Lab
lab = Lab <$> L.identifier

longid :: ([Id] -> Id -> a) -> Parser a
longid f = do
  (q, i) <- L.qualifiedid
  return $ f q i

longvid :: Parser VId
longvid = longid QVId <|> vid

longtycon :: Parser TyCon
longtycon = longid QTyCon <|> tycon

