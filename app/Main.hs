module Main where

import qualified Ily.Lexer as L
import qualified Ily.Parser as P
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
    setLocaleEncoding utf8
    getContents >>= (putStrLn . show . L.run P.parseProg)
