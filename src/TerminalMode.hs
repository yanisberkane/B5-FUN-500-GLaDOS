module TerminalMode (startTerminalSession) where

import System.IO (hFlush, stdout)
import ParserSExpr (stringToSExpr)
import Interpreter (evaluate)
import Types (Env, SExpr, Ast, sexprToAst)
import ErrorHandler (formatError, formatResult)
import qualified Data.Map as Map
import Control.Monad (unless)

startTerminalSession :: IO ()
startTerminalSession = terminalLoop Map.empty

terminalLoop :: Env -> IO ()
terminalLoop env = do
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (input == "exit") $ do
        case stringToSExpr input of
            Just [sexpr] -> processInput env sexpr >>= terminalLoop
            _ -> terminalLoop env

processInput :: Env -> SExpr -> IO Env
processInput env sexpr = do
    let maybeAst = sexprToAst sexpr
    case maybeAst of
        Just ast ->
            case evaluate env ast of
                Left err -> do
                    putStrLn (formatError err)
                    return env
                Right (result, newEnv) -> do
                    maybe (return ()) putStrLn (formatResult result)
                    return newEnv
        Nothing -> return env