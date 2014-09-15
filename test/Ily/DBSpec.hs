module Ily.DBSpec
        ( spec
        ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, bracket)

import Database.HDBC
import Database.HDBC.Sqlite3



import Ily.DB

spec :: Spec
spec = do
    describe "DB" $ do
        it "can get connect" $
            ds runGetTables `shouldReturn` ["test"]
        where
            ds = runDataSource ":memory:"


runGetTables c = do
            _ <- runRaw c "CREATE TABLE test (id INTEGER);"
            getTables c
