module Main where

import qualified Ily.Parser as P
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
    setLocaleEncoding utf8
    getContents >>= P.parseTest P.decs
