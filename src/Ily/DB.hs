{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Ily.DB 
    ( createSchema
    ) where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name String
    description String Maybe 
    deriving Show
Release
    name String
    description String Maybe
    project_id ProjectId
    deriving Show
Issue
    title String
    description String Maybe
    release_id ReleaseId
    deriving Show
Record
    comments String Maybe
    started_on Int
    finished_on Int
    issue_id IssueId
    deriving Show
|]

createSchema :: IO ()
createSchema = runSqlite ":memory:" $ do
        runMigration migrateAll
