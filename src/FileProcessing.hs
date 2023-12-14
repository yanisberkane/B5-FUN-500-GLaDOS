module FileProcessing (processFile, processContent) where
import Control.Monad
import ErrorHandler
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.FilePath (takeExtension)
import ParserSExpr (stringToSExpr)
import Types (Env, SExpr, Ast)
import Interpreter (evaluate, sExprToAst)
import qualified Data.Map as Map

processFile :: FilePath -> IO ()
processFile filename
    | takeExtension filename /= ".glados" = handleError $ FileError "Invalid file extension. Expecting '.glados'"
    | otherwise = readFile filename `catchIOError` handleFileReadError >>= processContent

processSExpr :: Env -> SExpr -> IO Env
processSExpr env sexpr = do
    let ast = sExprToAst sexpr
    case evaluate env ast of
        Left error -> print error >> return env
        Right (result, newEnv) -> print result >> return newEnv

processContent :: String -> IO ()
processContent content
    | null content = handleError $ FileError "No input provided."
    | otherwise = do
        case stringToSExpr content of
            Just sexprs -> foldM processSExpr Map.empty sexprs >> return ()
            Nothing -> handleError $ ParsingError "Failed to parse the content."

handleFileReadError :: IOError -> IO String
handleFileReadError e
    | isDoesNotExistError e = do
        handleError (FileError "File does not exist.")
        return ""
    | otherwise = do
        handleError (FileError $ "Error reading file: " ++ show e)
        return ""

evaluateExpressions :: [SExpr] -> Env -> IO ()
evaluateExpressions [] _ = return ()
evaluateExpressions (sexpr:sexprs) env = do
    let ast = sExprToAst sexpr
    case evaluate env ast of
        Left error -> print error
        Right (result, newEnv) -> do
            print result
            evaluateExpressions sexprs newEnv
