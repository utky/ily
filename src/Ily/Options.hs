module Ily.Options
    ( Options (..)
    -- , compileOptions
    ) where


import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative


{-- | Global Command line options
-}
data Options = Options
            { version :: Bool
            , verbose :: Bool
            }


{-
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

-}
