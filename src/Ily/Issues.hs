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

class Container a m where
    contents :: a -> m b

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


releases :: Project -> [Release]
releases = undefined

issues :: Release -> [Issue]
issues = undefined

records :: Issue -> [Record]
records = undefined
