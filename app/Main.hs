module Main where

import Ily.Lexer
import Ily.Parser

main :: IO ()
main = go where
  go = do
    getLine >>= (putStrLn . show . run parseScons) >> go
