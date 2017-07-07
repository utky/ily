module Ily.LexerSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import qualified Ily.Lexer as L

shouldScan :: String -> L.Token -> Expectation
shouldScan s t = L.run L.scanner s `shouldBe` (Right t)

spec :: Spec
spec = do

  it "scans 1 digit integer" $ 
    "1" `shouldScan` (L.TInt 1)
  it "scans 2 digit integer" $
    "12" `shouldScan` (L.TInt 12)
  it "scans negative integer" $
    "-1" `shouldScan` (L.TInt (-1))

  it "scans alphabet string" $
    "\"a\"" `shouldScan` (L.TStr "a")
  it "scans alphabet with newline string" $
    "\"a\nb\"" `shouldScan` (L.TStr "a\nb")
  it "scans alphabet with carrige return string" $
    "\"a\r\nb\"" `shouldScan` (L.TStr "a\r\nb")
