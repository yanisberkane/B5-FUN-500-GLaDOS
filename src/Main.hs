module Main where

import System.IO
import System.Environment
import Control.Monad
import FileProcessing
import TerminalMode

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        [] -> do
            inputAvailable <- hReady stdin
            if inputAvailable
                then readAndProcessStdin
                else startTerminalSession
        _ -> putStrLn "Error: Invalid arguments"

readAndProcessStdin :: IO ()
readAndProcessStdin = do
    content <- getContents
    processContent content