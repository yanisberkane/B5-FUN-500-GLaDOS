{-
-- EPITECH PROJECT, 2024
-- B5-FUN-500-GLaDOS-Mirror [WSL: Ubuntu]
-- File description:
-- Main
-}
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
import Types
import VMTypes

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filename] | takeExtension filename == ".ccs" -> processFile filename False
        ["-d", filename] | takeExtension filename == ".ccs" -> processFile filename True
        _ -> handleError $ InvalidArgumentError "Invalid arguments."

processFile :: FilePath -> Bool -> IO ()
processFile filename debugMode =
    try (readFile filename) >>= either handleReadError (\input -> processInput filename input debugMode)

handleReadError :: IOException -> IO ()
handleReadError e = handleError $ InvalidArgumentError ("File error: " ++ show e)

processInput :: FilePath -> String -> Bool -> IO ()
processInput filename input debugMode =
    case bufferToCCSAst input of
        Just ast -> do
            let result = interpretAST ast
            case result of
                Left err -> handleError err
                Right (env, insts) ->
                    let outputFilename = takeBaseName filename <.> "dz"
                    in (if debugMode
                        then writeDebugInfo filename (env, insts)
                        else putStrLn (filename ++ " was compiled successfully. ^_^") >>
                             writeEnvAndInstsToFile outputFilename (env, insts))
        Nothing -> handleError $ ParsingError "Failed to parse .ccs file."

writeDebugInfo :: FilePath -> (VMEnv, Insts) -> IO ()
writeDebugInfo filename (env, insts) = do
let baseName = takeBaseName filename
let debugFilename = baseName ++ ".debug"
let debugContent = "Environment:\n" ++ formatEnv env ++ "\nInstructions:\n" ++ formatInsts insts
writeFile debugFilename debugContent
putStrLn $ "Debug info written to " ++ debugFilename