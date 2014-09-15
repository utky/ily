module Ily.CommandsSpec
        ( spec
        ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Options.Applicative as Op

import Ily.Commands (Command(..), parserInfo)

spec :: Spec
spec = do
    describe "Commands" $ do
        it "can read subcommand" $
            assertResult (run parserInfo ["version"]) $ \c -> c `shouldBe` Version
        -- it "is inverse to show" $ property $
        --     \x -> (read . show) x == (x :: Int)


run :: ParserInfo a -> [String] -> ParserResult a
run = execParserPure (prefs idm)

assertResult :: ParserResult a -> (a -> Expectation) -> Expectation
assertResult x f = case x of
  Op.Success r -> f r
  Op.Failure _ -> expectationFailure "Parser result failure"
  Op.CompletionInvoked _ -> expectationFailure "completion invoked"
