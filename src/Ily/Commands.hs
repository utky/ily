module Ily.Commands
                    ( Command(..)
                    , parser
                    , parserInfo
                    , buildCommand
                    ) where

import Ily.Configuration
import Ily.Options (Options)
import Ily.Flags (Flag)
import Ily.Core (Source, Target)

import Options.Applicative



data Runtime = Runtime Configuration Command 

{-- | A basic data structure of ily subcommand
- 
-}

{-- | Domain specific command for ily
-}
data Command =
        Version
        | Init 
        | Compile
        | Build
        | Package
        | Ticket
        deriving (Show, Eq)

{-- | 
 -}
data IsCommand =
        List
        | Add
        | Delete

buildRuntime :: Command -> Configuration -> Runtime
buildRuntime cmd cfg = Runtime cfg cmd

-- | 
-- コマンドの定義をもらう
-- -> 定義からruntimeのオプションをパースする
-- -> デフォルトオプションにマージする
-- -> 実行可能なコマンドができあがる
buildCommand :: Options -> [String] -> Command
buildCommand o p = undefined


runCommand :: Command -> Source a -> Target b
runCommand c i = undefined

common = undefined

version :: Parser Command
version = pure Version

parser :: Parser Command
parser =  subparser
    ( 
        command "version" (info version (progDesc "indicate appilcation version"))
    -- <>  command "init"    (info (progDesc "initialize current directory"))
    )

parserInfo :: ParserInfo Command
parserInfo = info parser ( progDesc "interactive shell for document development." )
 
