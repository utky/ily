-- | Provides fundamental data models
module Ily.Core
    (
      Source(..)
    , Target(..)
    ) where

data Source a = MkSource a

data Target a = MkTarget a
