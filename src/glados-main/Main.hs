module Main where

import System.IO
import System.Environment
import Control.Exception (catch, IOException, try)
import System.FilePath (takeBaseName, takeExtension, (<.>))
import CCSAstParser (bufferToCCSAst)
import ErrorHandler (handleError, ErrorType(ParsingError, InvalidArgumentError))
import Interpreter (interpretAST)
import FileProcessor (writeEnvAndInstsToFile)
import Formatter (formatEnv, formatInsts)

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filename] | takeExtension filename == ".ccs" -> processFile filename
                   | otherwise -> handleError $ ParsingError "Invalid file extension. Expected '.ccs' >:C."
        _ -> handleError $ InvalidArgumentError "File was not provided."

processFile :: FilePath -> IO ()
processFile filename =
    try (readFile filename) >>= either handleReadError (\input -> processInput filename input)

handleReadError :: IOException -> IO ()
handleReadError e = handleError $ InvalidArgumentError ("File error: " ++ show e)

processInput :: FilePath -> String -> IO ()
processInput filename input =
    case bufferToCCSAst input of
        Just ast -> do
            putStrLn "AST tree:"
            print ast
            let (env, insts) = interpretAST ast
            putStrLn "Environment:"
            putStr $ formatEnv env
            putStrLn "Instructions:"
            putStr $ formatInsts insts
            let outputFilename = takeBaseName filename <.> "dz"
            writeEnvAndInstsToFile outputFilename (env, insts)
        Nothing  -> handleError $ ParsingError "Failed to parse .ccs file."