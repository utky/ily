module Ily.DBSpec
        ( spec
        ) where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate, bracket)

import Data.List (sort)
import Database.HDBC (getTables, toSql)
-- import Database.HDBC.Sqlite3



import Ily.DB

type TableName = String

spec :: Spec
spec = do
    describe "DB" $ do
        it "can create tables" $
            datasource runGetTables >>= \ts -> 
                tablenames ts `shouldBe` sort ["projects", "releases", "issues", "records"]
        it "can clean up tables" $
            datasource cleanTables >>= \ts ->
                tablenames ts `shouldBe` []
        it "can insert and select data" $
            datasource insert >>= \cnt ->
                cnt `shouldBe` 1
        where
            datasource = runDataSource ":memory:"
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

insert :: Query Integer
insert = do
        initializeSchema
        newProject <- statement "INSERT INTO projects(name, description) VALUES (?, ?)"
        cnt <- executeStatement newProject [toSql "hello", toSql "world"]
        dbcommit
        return cnt
        -- projectsRow <- statement "SELECT * FROM projects"
        -- rows <- listStatement projectsRow []
        -- return $ length rows



