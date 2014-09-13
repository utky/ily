module Ily.Configuration 
    ( Configuration(..)
    , projectConfig
    ) where

-- import Data.Default         (Default (..))
import System.Directory     ( getCurrentDirectory
                            , getHomeDirectory)
import System.FilePath      ((</>))


data Configuration = Configuration
    { -- |
      baseDirectory :: FilePath
    , -- |
      projectConfigDirectory :: FilePath
    , -- |
      globalConfigDirectory :: FilePath
    , -- |
      destinationDirectory :: FilePath
    , -- |
      sourceDirectory :: FilePath

    } deriving (Show, Eq)

{-
instance Default Configuration where
    def = defaultConfig
-}


projectConfig :: IO Configuration
projectConfig = do
        cd      <- getCurrentDirectory
        configd <- return $ cd </> dotIly
        globald <- getHomeDirectory >>= \hd -> return $ hd </> dotIly
        return Configuration
            { baseDirectory = cd
            , projectConfigDirectory = configd
            , globalConfigDirectory = globald
            , destinationDirectory = cd </> "target"
            , sourceDirectory = cd </> "src"
            }
        where
            dotIly = ".ily"


{--
defaultConfig :: Configuration
defaultConfig = Configuration
    {
        baseDirectory = bd
        , globalContextDirectory = "${HOME}"
        , destinationDirectory = bd </> "target"
        , sourceDirectory = bd </> "src"
    }
    where 
          bd = "."
-}
