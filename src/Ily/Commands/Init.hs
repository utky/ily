module Ily.Commands.Init
    (
      createIlyDir
    ) where


import System.Directory (createDirectoryIfMissing)

import Ily.Configuration (Configuration(..))

createIlyDir :: Configuration -> IO ()
createIlyDir c = do 
    createDirectoryIfMissing True ilyDir
    where
        ilyDir = projectConfigDirectory c


