module Ily.Compiler () where


newtype Source 

data Content a

data Document

compile :: ()

sources :: FilePath -> [Source]

read :: Source -> Content a

write :: Document -> IO ()

parse :: Content a -> Document


