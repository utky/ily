module Ily.DB 
    ( Query
    , raw
    , action
    , dbcommit
    , runDataSource
    , initializeSchema
    , cleanSchema
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import System.FilePath
import Paths_ily

import Control.Exception as E

import Ily.Configuration (dataSourcePath)


newtype Query e = MkQuery { runQuery :: Connection -> IO e }

instance Monad Query where
        return e = MkQuery $ \_ -> return e
        ma >>= f = MkQuery $ \c -> do
            next <- ioresult c
            (runQuery $ f next) c
            where
                ioresult c = runQuery ma c

-- class Entity e where
--     newEntity :: e -> Query e
--     deleteEntity :: e => e -> Query e



runDataSource :: String -> Query a -> IO a
runDataSource datapath query = bracket
        (connectSqlite3 datapath)
        (disconnect)
        (handler)
        where
            handler = runQuery query

dbcommit = action $ \c -> do
    commit c
    return ()

{- | Wrapper for HDBC interfaces
-}
action :: (Connection -> IO a) -> Query a
action f = MkQuery f

{- | Execution raw query statement
-}
raw :: String -> Query ()
raw s = MkQuery $ \c -> runRaw c s
        
{- | Execute DDL
-}
initializeSchema :: Query ()
initializeSchema = MkQuery $ \c -> do
        schema <- schemaDefinition
        runRaw c schema
        return ()

cleanSchema :: Query ()
cleanSchema = raw $ qBody
        where
            tables = ["projects", "releases", "issues", "records"]
            tablename t = "DROP TABLE " ++ t ++ " ;"
            qBody = foldr (++) "" $ map tablename tables

schemaDefinition :: IO String
schemaDefinition = do
        sfile <- getDataFileName $ "sql" </> "schema.sql"
        readFile sfile
