module Ily.CommandsSpec
        ( spec
        ) where

import Test.Hspec
-- import Test.QuickCheck
import Control.Exception (evaluate)

import Options.Applicative

import Ily.Commands (Command(..), parserInfo)

spec :: Spec
spec = do
    describe "Commands" $ do
        it "can read subcommand" $
            assertResult result $ \c -> c `shouldBe` Version
    where
        args = ["version"]
        parse = run parserInfo
        result = parse args


run :: ParserInfo a -> [String] -> ParserResult a
run = execParserPure (prefs idm)

assertResult :: ParserResult a -> (a -> Expectation) -> Expectation
assertResult x f = case x of
  Success r -> f r
  Failure _ -> expectationFailure "Parser result failure"
  CompletionInvoked _ -> expectationFailure "completion invoked"
