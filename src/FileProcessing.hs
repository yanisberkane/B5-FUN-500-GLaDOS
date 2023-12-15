module FileProcessing (processFile, processContent) where

import Control.Monad (foldM)
import ErrorHandler (formatError, handleError, formatResult, ErrorType(..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.FilePath (takeExtension)
import ParserSExpr (stringToSExpr)
import Types (Ast(..), SExpr(..), Env)
import Interpreter (evaluate, sExprToAst)
import qualified Data.Map as Map
import System.Exit (exitFailure)

processFile :: FilePath -> IO ()
processFile filename
    | takeExtension filename /= ".scm" = handleError $ FileError "Invalid file extension. Expecting '.scm'"
    | otherwise = readFile filename `catchIOError` handleFileReadError >>= processContent

processContent :: String -> IO ()
processContent content
    | null content = handleError $ FileError "No input provided."
    | otherwise = maybe (handleError $ ParsingError "Failed to parse the content.") (processExpressions Map.empty) (stringToSExpr content)

processExpressions :: Env -> [SExpr] -> IO ()
processExpressions env [] = handleError $ ParsingError "No expressions to evaluate."
processExpressions env sexprs = do
    finalEnv <- foldM processSExpr env (init sexprs)
    evaluateAndPrint finalEnv (last sexprs)

processSExpr :: Env -> SExpr -> IO Env
processSExpr env sexpr = case evaluate env (sExprToAst sexpr) of
    Left err -> handleError err >> return env
    Right (_, newEnv) -> return newEnv

evaluateAndPrint :: Env -> SExpr -> IO ()
evaluateAndPrint env sexpr = case evaluate env (sExprToAst sexpr) of
    Left err -> handleError err
    Right (result, _) -> maybe (return ()) putStrLn (formatResult result)

handleFileReadError :: IOError -> IO String
handleFileReadError e = do
    putStrLn $ formatError $ if isDoesNotExistError e
        then FileError "File does not exist."
        else FileError $ "Error reading file: " ++ show e
    exitFailure
    return ""