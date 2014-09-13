module Main (main) where

import              System.Environment  (getArgs)
import              Data.Default        (def)

import              Ily.Commands        (Runtime, Command, buildCommand, buildRuntime)
import              Ily.Options         (Options)
import              Ily.Configuration   

main :: IO ()
main = do
    (opts, args) <- getArgs >>= compileOptions
    runtime opts args defaultCfg
    return ()
    where
        defaultCfg = def
        runtime = buildRuntime . buildCommand
