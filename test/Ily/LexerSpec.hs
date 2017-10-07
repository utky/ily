module Ily.LexerSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Ily.Lexer as L

shouldScan :: String -> L.Token -> Expectation
shouldScan s t = L.run L.scanner s `shouldBe` (Right t)

shouldFail :: String -> Expectation
shouldFail s = L.run L.scanner s `shouldSatisfy` isFailure where
  isFailure (Right _) = False
  isFailure (Left _) = True

spec :: Spec
spec = do

  describe "integer" $ do
    it "scans 1 digit integer" $ 
      "1" `shouldScan` (L.TInt 1)
    it "scans 2 digit integer" $
      "12" `shouldScan` (L.TInt 12)
    it "scans negative integer" $
      "~1" `shouldScan` (L.TInt (-1))

  describe "string" $ do
    it "scans alphabet string" $
      "\"a\"" `shouldScan` (L.TStr "a")
    it "scans alphabet with newline string" $
      "\"a\nb\"" `shouldScan` (L.TStr "a\nb")
    it "scans alphabet with carrige return string" $
      "\"a\r\nb\"" `shouldScan` (L.TStr "a\r\nb")
    it "scans multibyte string" $
      "\"文字列\"" `shouldScan` (L.TStr "文字列")
    it "scans multibyte with newline string" $
      "\"文字列\nalpha123\"" `shouldScan` (L.TStr "文字列\nalpha123")

  describe "character" $ do
    it "scans alphabet char" $
      "#\"a\"" `shouldScan` (L.TChar 'a')
    it "scans numeric char" $
      "#\"1\"" `shouldScan` (L.TChar '1')
    it "scans multibyte char" $
      "#\"字\"" `shouldScan` (L.TChar '字')

  describe "reserved" $ do
    it "scans (" $
      "(" `shouldScan` L.TLParen
    it "scans )" $
      ")" `shouldScan` L.TRParen
    it "scans [" $
      "[" `shouldScan` L.TLBracket
    it "scans ]" $
      "]" `shouldScan` L.TRBracket
    it "scans {" $
      "{" `shouldScan` L.TLBrace
    it "scans }" $
      "}" `shouldScan` L.TRBrace
    it "scans ," $
      "," `shouldScan` L.TComma
    it "scans :" $
      ":" `shouldScan` L.TColon
    it "scans ;" $
      ";" `shouldScan` L.TSemiColon
    it "scans ..." $
      "..." `shouldScan` L.TRowWild
    it "scans _" $
      "_" `shouldScan` L.TValWild
    it "scans |" $
      "|" `shouldScan` L.TBar
    it "scans =" $
      "=" `shouldScan` L.TEq
    it "scans =>" $
      "=>" `shouldScan` L.TFatArrow
    it "scans ->" $
      "->" `shouldScan` L.TArrow
    it "scans #" $
      "#" `shouldScan` L.TSharp

    it "scans abstype" $
      "abstype" `shouldScan` L.TAbsType
    it "scans and" $
      "and" `shouldScan` L.TAnd
    it "scans andalso" $
      "andalso" `shouldScan` L.TAndAlso
    it "scans as" $
      "as" `shouldScan` L.TAs
    it "scans case" $
      "case" `shouldScan` L.TCase
    it "scans datatype" $
      "datatype" `shouldScan` L.TDataType
    it "scans do" $
      "do" `shouldScan` L.TDo
    it "scans else" $
      "else" `shouldScan` L.TElse
    it "scans end" $
      "end" `shouldScan` L.TEnd
    it "scans exception" $
      "exception" `shouldScan` L.TException
    it "scans fn" $
      "fn" `shouldScan` L.TFn
    it "scans fun" $
      "fun" `shouldScan` L.TFun
    it "scans handle" $
      "handle" `shouldScan` L.THandle
    it "scans if" $
      "if" `shouldScan` L.TIf
    it "scans in" $
      "in" `shouldScan` L.TIn
    it "scans infix" $
      "infix" `shouldScan` L.TInfix
    it "scans infixr" $
      "infixr" `shouldScan` L.TInfixr
    it "scans let" $
      "let" `shouldScan` L.TLet
    it "scans local" $
      "local" `shouldScan` L.TLocal
    it "scans nonfix" $
      "nonfix" `shouldScan` L.TNonfix
    it "scans of" $
      "of" `shouldScan` L.TOf
    it "scans op" $
      "op" `shouldScan` L.TOp
    it "scans open" $
      "open" `shouldScan` L.TOpen
    it "scans orelse" $
      "orelse" `shouldScan` L.TOrElse
    it "scans raise" $
      "raise" `shouldScan` L.TRaise
    it "scans rec" $
      "rec" `shouldScan` L.TRec
    it "scans then" $
      "then" `shouldScan` L.TThen
    it "scans type" $
      "type" `shouldScan` L.TType
    it "scans val" $
      "val" `shouldScan` L.TVal
    it "scans with" $
      "with" `shouldScan` L.TWith
    it "scans withtype" $
      "withtype" `shouldScan` L.TWithType
    it "scans while" $
      "while" `shouldScan` L.TWhile

  describe "identifier" $ do
    it "scans alphabet identifier" $
      "a" `shouldScan` (L.TId "a")
    it "scans alphabet and digit identifier" $
      "a1" `shouldScan` (L.TId "a1")
    it "scans alphabet and underscore and digit identifier" $
      "a_1" `shouldScan` (L.TId "a_1")
    it "scans alphabet and prime identifier" $
      "a'" `shouldScan` (L.TId "a'")
    it "scans multibyte identifier" $
      "識別子" `shouldScan` (L.TId "識別子")
    it "scans mixed identifier" $
      "識別子_a1" `shouldScan` (L.TId "識別子_a1")
    -- it "doesn't scan digit headed identifier" $
    --   shouldFail "1識別子"
    it "scans primed reserved identifier" $
      "type'" `shouldScan` (L.TId "type'")
    it "scans && identifier" $
      "&&" `shouldScan` (L.TId "&&")
    it "scans || identifier" $
      "||" `shouldScan` (L.TId "||")

  describe "long identifier" $ do
    it "scans alphabet identifier" $
      "a.b.c" `shouldScan` (L.TLongId ["a", "b", "c"])
    it "scans large alphabet identifier" $
      "Alpha.Beta.Gamma" `shouldScan` (L.TLongId ["Alpha", "Beta", "Gamma"])
