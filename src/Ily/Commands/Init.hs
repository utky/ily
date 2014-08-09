module Ily.Commands.Init
    (
      createIlyDir
    ) where

import System.FilePath ((</>))
import System.Directory

createIlyDir :: FilePath -> IO ()
createIlyDir target = do 
    createDirectoryIfMissing True ilyDir
    where
        ilyDir = target </> ".ily"


