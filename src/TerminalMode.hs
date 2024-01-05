module TerminalMode (startTerminalSession) where

import System.IO (hFlush, stdout)
import CCSParser (bufferToCCS)
import Interpreter (evaluate)
import Types (Env, CCS, Ast, ccsToAst)
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
        case bufferToCCS input of
            Just [ccs] -> processInput env ccs >>= terminalLoop
            _ -> terminalLoop env

processInput :: Env -> CCS -> IO Env
processInput env sexpr = do
    let maybeAst = ccsToAst sexpr
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