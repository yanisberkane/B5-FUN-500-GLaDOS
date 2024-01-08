module Main where

import System.IO
import System.Environment
import Control.Exception (catch, IOException, try)
import CCSAstParser (bufferToCCSAst)
import ErrorHandler (handleError, ErrorType(ParsingError, InvalidArgumentError))
import System.FilePath (takeExtension)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] | takeExtension filename == ".ccs" -> processFile filename
                   | otherwise -> handleError $ ParsingError "Invalid file extension. Expected '.ccs' >:C."
        _ -> handleError $ InvalidArgumentError "File was not provided."

processFile :: FilePath -> IO ()
processFile filename =
    try (readFile filename) >>= either handleReadError processInput

handleReadError :: IOException -> IO ()
handleReadError e = handleError $ InvalidArgumentError ("File error: " ++ show e)

processInput :: String -> IO ()
processInput input =
    case bufferToCCSAst input of
        Just ast -> print ast
        Nothing  -> handleError $ ParsingError "Failed to parse .ccs file."