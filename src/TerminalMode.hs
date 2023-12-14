module TerminalMode (startTerminalSession) where

import System.IO (hFlush, stdout)
import ParserSExpr (stringToSExpr)
import Interpreter (evaluate, sExprToAst)
import Types (Env, SExpr(..), Ast(..))
import ErrorHandler (ErrorType(..))
import qualified Data.Map as Map
import Control.Monad (unless, foldM)

startTerminalSession :: IO ()
startTerminalSession = terminalLoop Map.empty

--TEMPORARY ?
processSExpr :: Env -> SExpr -> IO Env
processSExpr env sexpr = do
    let ast = sExprToAst sexpr
    case evaluate env ast of
        Left error -> do
            putStrLn $ formatError error
            return env
        Right (result, newEnv) -> do
            maybe (return ()) putStrLn (formatResult result)
            return newEnv

formatError :: ErrorType -> String
formatError (UndefinedVariableError varName) = "*** ERROR : variable " ++ varName ++ " is not bound."
formatError err = show err

formatResult :: Ast -> Maybe String
formatResult (AstInt n) = Just $ show n
formatResult (Define _ _ _) = Nothing
formatResult _ = Nothing

terminalLoop :: Env -> IO ()
terminalLoop env = do
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (input == "exit") $ do
        case stringToSExpr input of
            Just [sexpr] -> processSExpr env sexpr >>= terminalLoop
            _ -> putStrLn "Invalid input or multiple expressions" >> terminalLoop env