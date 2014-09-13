module Ily.ConfigurationSpec (spec) where

import Ily.Configuration ( Configuration
                         , projectConfig
                         )

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "Configuration" $ do
        it "build from current directory" $
            read "10" `shouldBe` (10 :: Int)

