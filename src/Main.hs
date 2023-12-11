module Main where
import System.IO
import Control.Monad
import System.Environment
import FileProcessing
import TerminalMode

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        []         -> startTerminalSession