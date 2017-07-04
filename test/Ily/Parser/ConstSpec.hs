module Ily.Parser.ConstSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import qualified Ily.Parser as P
import qualified Ily.Parser.Const as C


spec :: Spec
spec = do
  it "parse string" $ do
    P.runParser C.scons "" "\"string\""
      `shouldBe` (Right (C.SStr "string"))
