module Main where
import System.IO
import Control.Monad
import System.Environment
import FileProcessing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        []         -> putStrLn "Terminal mode not implemented yet."