module Ily.DB 
    ( runDataSource
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Exception as E

import Ily.Configuration (Configuration, dataSourcePath)

data DataSource = SqliteDataSource String

-- newtype Query e = MkQuery { runQuery :: Connection -> IO e }

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

runDataSource :: String -> (Connection -> IO a) -> IO a
runDataSource datapath handler = bracket
        (connectSqlite3 datapath)
        (disconnect)
        (handler)

memory :: DataSource
memory = SqliteDataSource ":memory:"

mkDataSource :: Configuration -> DataSource
mkDataSource cfg = SqliteDataSource dpath
    where
        dpath = dataSourcePath cfg


