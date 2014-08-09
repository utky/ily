module Ily.Options(
                  Options (..)
                  , compileOptions
                  ) where

import System.Console.GetOpt
import Data.Maybe ( fromMaybe ) 

data Options = Options {
                  optVersion :: Bool
                , optVerbose :: Bool
            }

defaultOptions :: Options
defaultOptions = Options {
                  optVersion = False
                , optVerbose = False
            }

options :: [OptDescr (Options -> Options)]
options = 
    [
        Option ['v'] ["verbose"] (NoArg (\ opts -> opts { optVerbose = True })) "chatty output on stderr"
      , Option ['V'] ["version"] (NoArg (\ opts -> opts { optVersion = True })) "show version number"
    ]
 
compileOptions :: [String] -> IO (Options, [String])
compileOptions argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: ic [OPTION...] files..."
