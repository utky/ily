module Ilya.Command () where

import Ilya.Config

class Command a where
    run :: Command a => a -> Context => IO ()
