module Ily.LexerSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Ily.Parser as P
import           Control.Monad (forM_)

shouldParse :: (Eq a, Show a) => P.Parser a -> String -> a -> Expectation
shouldParse p i e = P.runParser p "none" i `shouldBe` Right e

spec :: Spec
spec = do

  describe "integer" $ do
    it "scans 1 digit integer" $ 
      shouldParse P.integer "1" 1
    it "scans 2 digit integer" $
      shouldParse P.integer "12" 12
    it "scans negative integer" $
      shouldParse P.integer "~1" (-1)

  describe "string" $ do
    it "scans alphabet string" $
      shouldParse P.string "\"a\"" "a"
    it "scans alphabet with newline string" $
      shouldParse P.strings "`a\nb`" "a\nb"
    it "scans alphabet with carrige return string" $
      shouldParse P.strings "`a\r\nb`" "a\r\nb"
    it "scans multibyte string" $
      shouldParse P.string "\"文字列\"" "文字列"
    it "scans multibyte with newline string" $
      shouldParse P.strings "`文字列\nalpha123`" "文字列\nalpha123"

  describe "character" $ do
    it "scans alphabet char" $
      shouldParse P.character "#\"a\"" 'a'
    it "scans numeric char" $
      shouldParse P.character "#\"1\"" '1'
    it "scans multibyte char" $
      shouldParse P.character "#\"字\""'字'

  describe "reserved" $ do
    let tokens = [ (P.lparen, "(")
                 , (P.rparen, ")")
                 , (P.lbracket, "[")
                 , (P.rbracket, "]")
                 , (P.lbrace, "{")
                 , (P.rbrace, "}")
                 , (P.comma, ",")
                 , (P.colon, ":")
                 , (P.semicolon, ";")
                 , (P.tridot, "...")
                 , (P.underscore, "_")
                 , (P.bar, "|")
                 , (P.eq, "=")
                 , (P.arrow, "->")
                 , (P.fatArrow, "=>")
                 , (P.abstype, "abstype")
                 , (P.and, "and")
                 , (P.andalso, "andalso")
                 , (P.as, "as")
                 , (P.case', "case")
                 , (P.datatype, "datatype")
                 , (P.do', "do")
                 , (P.else', "else")
                 , (P.end, "end")
                 , (P.exception, "exception")
                 , (P.fn, "fn")
                 , (P.fun, "fun")
                 , (P.handle, "handle")
                 , (P.if', "if")
                 , (P.in', "in")
                 , (P.infix', "infix")
                 , (P.infixr', "infixr")
                 , (P.let', "let")
                 , (P.local, "local")
                 , (P.nonfix, "nonfix")
                 , (P.of', "of")
                 , (P.op, "op")
                 , (P.open, "open")
                 , (P.orelse, "orelse")
                 , (P.raise, "raise")
                 , (P.rec, "rec")
                 , (P.then', "then")
                 , (P.type', "type")
                 , (P.val, "val")
                 , (P.with, "with")
                 , (P.withtype, "withtype")
                 , (P.while, "while")
                 ]
        check (p, t) = it ("scans " ++ t) $ shouldParse p t t
    forM_ tokens check

  describe "identifier" $ do
    it "scans alphabet identifier" $
      shouldParse P.identifier "a" "a"
    it "scans alphabet and digit identifier" $
      shouldParse P.identifier "a1" "a1"
    it "scans alphabet and underscore and digit identifier" $
      shouldParse P.identifier "a_1" "a_1"
    it "scans alphabet and prime identifier" $
      shouldParse P.identifier "a'""a'"
    it "scans multibyte identifier" $
      shouldParse P.identifier "識別子" "識別子"
    it "scans mixed identifier" $
      shouldParse P.identifier "識別子_a1" "識別子_a1"
    -- it "doesn't scan digit headed identifier" $
    --   shouldFail "1識別子"
    it "scans primed reserved identifier" $
      shouldParse P.identifier "type'" "type'"
    it "scans && identifier" $
      shouldParse P.operator "&&" "&&"
    it "scans || identifier" $
      shouldParse P.operator "||" "||"

  describe "long identifier" $ do
    it "scans alphabet identifier" $
      shouldParse P.qualifiedid "a.b.c" (["a", "b"], "c")
    it "scans large alphabet identifier" $
      shouldParse P.qualifiedid "Alpha.Beta.Gamma" (["Alpha", "Beta"], "Gamma")
    it "scans multi byte identifier" $
      shouldParse P.qualifiedid "α.β.γ" (["α", "β"], "γ")
