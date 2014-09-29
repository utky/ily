module Ily.DBSpec
        ( spec
        ) where

import Test.Hspec

import Ily.DB (createSchema)

spec :: Spec
spec = do
    describe "DB" $ do
        it "can create tables" $
            createSchema `shouldReturn` ()

