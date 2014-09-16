module Ily.DBSpec
        ( spec
        ) where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate, bracket)

import Data.List (sort)
import Database.HDBC (getTables)
-- import Database.HDBC.Sqlite3



import Ily.DB

type TableName = String

spec :: Spec
spec = do
    describe "DB" $ do
        it "can create tables" $
            ds runGetTables >>= \ts -> 
                tablenames ts `shouldBe` sort ["projects", "releases", "issues", "records"]
        it "can clean up tables" $
            ds cleanTables >>= \ts ->
                tablenames ts `shouldBe` []
        where
            ds = runDataSource ":memory:"
            tablenames = sort . filter (\n -> n /= "sqlite_sequence")


runGetTables :: Query [TableName]
runGetTables = do
        initializeSchema
        action getTables

cleanTables :: Query [TableName]
cleanTables = do
        initializeSchema
        cleanSchema
        action getTables
