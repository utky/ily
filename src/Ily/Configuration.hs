module Ily.Configuration 
    (Configuration(..)) where


data Configuration = Configuration
    {
        baseDirectory :: FilePath
        , userContextDirectory :: FilePath
        , destinationDirectory :: FilePath
        , sourceDirectory :: FilePath
    }
