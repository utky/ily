-- | Provides fundamental data models
module Ily.Core
    ( Source
    , Target
    ) where

data Source a = MkSource a

data Target a = MkTarget a

data Query a = Select a

data Command a = New a
               | Modify a
               | Delete a

data Error = Error
           { summary :: String
           , detail :: String
           }

type Result a = Either Error a
