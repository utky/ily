module Ily.ParserSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Ily.Lexer as L
import qualified Ily.Parser as P
import qualified Ily.Syntax as S

shouldParse :: (Eq a, Show a) => L.P a -> String -> a -> Expectation
shouldParse p i s = L.run p i `shouldBe` (Right s)


spec :: Spec
spec = do

  describe "scons" $ do
    it "parses 1 digit integer" $ 
      shouldParse P.parseScons "1"  (S.SInt 1)
    it "parses string" $ 
      shouldParse P.parseScons "\"もじれつ\"" (S.SStr "もじれつ")
    it "parses character" $ 
      shouldParse P.parseScons "#\"字\"" (S.SChar '字')

  describe "dec" $ do
    it "int value" $ 
      shouldParse P.parseDec "val i = 1"
        (S.DVal []
          [(S.VBind (S.PAtPat (S.PVId S.Nop (S.Long [] (S.VId "i"))))
                   (S.EAtExp (S.ESCon (S.SInt 1))))])
