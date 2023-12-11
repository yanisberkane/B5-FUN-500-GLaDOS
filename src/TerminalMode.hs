module TerminalMode (startTerminalSession) where

import Control.Monad (unless)
import System.IO (hFlush, stdout)

startTerminalSession :: IO ()
startTerminalSession = do
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (input == "exit") $ do
        putStrLn "nothing made yet"
        startTerminalSession
