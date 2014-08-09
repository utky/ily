module Main (main) where

import Ily.Options

import System.Environment (getArgs)

import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)

main :: IO ()
main = do
    (opts, args) <- getArgs >>= compileOptions
    case args of 
        [] -> runInputT defaultSettings loop
        ["init"] -> undefined 
        _ -> printUsage
    where
        printUsage = putStrLn "Usage: init"

loop :: InputT IO ()
loop = do
    minput <- getInputLine "% "
    case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ "Input was: " ++ input
                         loop

