module Ily.Issues
    ( Project (..)
    , Release (..)
    , Issue (..)
    , Record (..)
    , releases
    , issues
    , records
    ) where

type Key = Integer

type Timestamp = Integer

type Issues = [Project]

data Project = Project
             { projectId :: Key
             , projectName :: String
             , projectReleases :: [Release]
             }

data Release = Release
             { releaseId :: Key
             , releaseName :: String
             , releaseIssues :: [Issue]
             }

data Issue = Issue
           { issueId :: Key
           , issueTitle :: String
           , issueDesc :: String
           , issueRecords :: [Record]
           }

data Record = Record
            { recordId :: Key
            , recordStart :: Timestamp
            , recordEnd :: Timestamp
            }
