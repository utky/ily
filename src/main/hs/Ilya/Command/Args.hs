module Ilya.Command.Args where

import System.Environment

data Arg

class Arg a where

instance Arg String where 


parseArgs :: [String] -> [Arg String]
parseArgs = map parseArg

parseArg :: String -> Args String
parseArg = 
