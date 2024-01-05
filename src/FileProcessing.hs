module FileProcessing (processFile, processContent) where

import Control.Monad (foldM)
import ErrorHandler (formatError, handleError, formatResult, ErrorType(..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.FilePath (takeExtension)
import CCSParser (bufferToCCS)
import Types (Ast(..), CCS, Env, ccsToAst)
import Interpreter (evaluate)
import qualified Data.Map as Map
import System.Exit (exitFailure)

processFile :: FilePath -> IO ()
processFile filename
    | takeExtension filename /= ".scm" = handleError $ FileError "Invalid file extension. Expecting '.scm'"
    | otherwise = readFile filename `catchIOError` handleFileReadError >>= processContent

processContent :: String -> IO ()
processContent content
    | null content = handleError $ FileError "No input provided."
    | otherwise = maybe (handleError $ ParsingError "Failed to parse the content.") (processExpressions Map.empty) (bufferToCCS content)

processExpressions :: Env -> [CCS] -> IO ()
processExpressions env [] = handleError $ ParsingError "No expressions to evaluate."
processExpressions env ccsExprs = do
    finalEnv <- foldM processSExpr env (init ccsExprs)
    evaluateAndPrint finalEnv (last ccsExprs)

processSExpr :: Env -> CCS -> IO Env
processSExpr env ccsExpr = case ccsToAst ccsExpr of
    Just ast ->
        case evaluate env ast of
            Left err -> handleError err >> return env
            Right (_, newEnv) -> return newEnv
    Nothing -> do
        handleError $ RuntimeError "Failed to parse CCS to Ast"
        return env

evaluateAndPrint :: Env -> CCS -> IO ()
evaluateAndPrint env ccsExpr = case ccsToAst ccsExpr of
    Just ast ->
        case evaluate env ast of
            Left err -> handleError err
            Right (result, _) -> maybe (return ()) putStrLn (formatResult result)
    Nothing -> handleError $ RuntimeError "Failed to parse CCS to Ast"

handleFileReadError :: IOError -> IO String
handleFileReadError e = do
    putStrLn $ formatError $ if isDoesNotExistError e
        then FileError "File does not exist."
        else FileError $ "Error reading file: " ++ show e
    exitFailure
    return ""