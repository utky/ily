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

  describe "pat" $ do
    it "atpat _" $ 
      shouldParse P.parsePat "_"  (S.PAtPat S.PWildcard)
    it "atpat op vid" $ 
      shouldParse P.parsePat "op >>" (S.PAtPat (S.PVId S.Op (S.VId ">>")))
    it "atpat nop vid" $ 
      shouldParse P.parsePat "value" (S.PAtPat (S.PVId S.Nop (S.VId "value")))
    it "atpat nop longvid" $ 
      shouldParse P.parsePat "Str.value" (S.PAtPat (S.PVId S.Nop (S.QVId ["Str"] "value")))
    it "atpat patrow empty" $ 
      shouldParse P.parsePat "{}"
        (S.PAtPat
          (S.PRec
            []))
    it "atpat patrow wildcard" $ 
      shouldParse P.parsePat "{...}"
        (S.PAtPat
          (S.PRec
            [S.PRWildcard]))
    it "atpat patrow" $ 
      shouldParse P.parsePat "{ value = 1 }"
        (S.PAtPat
          (S.PRec
            [(S.PRow
              (S.Lab "value")
              (S.PAtPat (S.PCon (S.SInt 1))))]))
    it "atpat patrows" $ 
      shouldParse P.parsePat "{ name = \"name\", age = 20 }"
        (S.PAtPat
          (S.PRec
            [(S.PRow
              (S.Lab "name")
              (S.PAtPat (S.PCon (S.SStr "name")))),
             (S.PRow
              (S.Lab "age")
              (S.PAtPat (S.PCon (S.SInt 20))))]))
    it "pat constructed pattern" $ 
      shouldParse P.parsePat "Just x"
        (S.PCtor S.Nop
          (S.VId "Just")
          (S.PVId S.Nop (S.VId "x")))
    it "pat infixed value construction" $ 
      shouldParse P.parsePat "x + y"
        (S.PInfix
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.VId "+")
          (S.PAtPat (S.PVId S.Nop (S.VId "y"))))
    it "pat no var typed" $ 
      shouldParse P.parsePat "x : t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyTyCon [] (S.TyCon "t")))
    it "pat var typed" $ 
      shouldParse P.parsePat "x : 'a t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyTyCon [(S.TyTyVar (S.TyVar "'a"))] (S.TyCon "t")))
    it "pat func typed" $ 
      shouldParse P.parsePat "x : s -> t"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyFunc
            (S.TyTyCon [] (S.TyCon "s"))
            (S.TyTyCon [] (S.TyCon "t"))))
    it "pat rec typed" $ 
      shouldParse P.parsePat "x : { f : t }"
        (S.PTyped
          (S.PAtPat (S.PVId S.Nop (S.VId "x")))
          (S.TyRec
            [(S.TyRow (S.Lab "f") (S.TyTyCon [] (S.TyCon "t")))]))

--  describe "dec" $ do
--    it "int value" $ 
--      shouldParse P.parseDec "val i = 1"
--        (S.DVal []
--          [(S.VBind (S.PAtPat (S.PVId S.Nop (S.VId "i")))
--                   (S.EAtExp (S.ESCon (S.SInt 1))))])
