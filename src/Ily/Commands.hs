module Ily.Commands (IlyCommand(..), buildCommand) where

import Ily.Configuration
import Ily.Options (Options)
import Ily.Flags (Flag)
import Ily.Core (Source, Target)

data IlyCommand = IlyCommand {
               commandName :: String
             , commandFlags :: [Flag]
             , commandOptions :: [Options]
             , commandDescription :: String
             , commandRun :: IO ()
             }

-- | 
-- コマンドの定義をもらう
-- -> 定義からruntimeのオプションをパースする
-- -> デフォルトオプションにマージする
-- -> 実行可能なコマンドができあがる
buildCommand :: Configuration -> [String] -> IlyCommand
buildCommand c p = undefined


runCommand :: IlyCommand -> Source a -> Target b
runCommand c i = undefined

