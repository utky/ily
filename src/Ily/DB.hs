module Ily.DB 
    ( Query
    , raw
    , action
    , runDataSource
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import System.FilePath
import Paths_ily

import Control.Exception as E

import Ily.Configuration (Configuration, dataSourcePath)

data DataSource = SqliteDataSource String

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


-- withDataSource :: DataSource -> Query e -> IO e
-- withDataSource ds q = withWConn (wconn ds) . \c -> runQuery q c
--         where
--             wconn (SqliteDataSource dp) = ConnWrapper $ connectSqlite3 dp
-- 
-- executeRaw :: String -> Query ()
-- executeRaw s = MkQuery $ \c -> runRaw c s

runDataSource :: String -> Query a -> IO a
runDataSource datapath query = bracket
        (connectSqlite3 datapath)
        (disconnect)
        (handler)
        where
            handler = runQuery query

action :: (Connection -> IO a) -> Query a
action f = MkQuery f

raw :: String -> Query ()
raw s = MkQuery $ \c -> runRaw c s
        

memory :: DataSource
memory = SqliteDataSource ":memory:"

mkDataSource :: Configuration -> DataSource
mkDataSource cfg = SqliteDataSource dpath
    where
        dpath = dataSourcePath cfg

initializeSchema :: Query ()
initializeSchema = MkQuery $ \c -> do
        schema <- schemaDefinition
        runRaw c schema
        return ()

schemaDefinition :: IO String
schemaDefinition = do
        sfile <- getDataFileName $ "sql" </> "schema.sql"
        readFile sfile
