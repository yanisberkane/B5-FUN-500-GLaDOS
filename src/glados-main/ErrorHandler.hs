module ErrorHandler (ErrorType(..), printError, formatError, handleError ,stringToErrorType) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Types (Ast(..))

data ErrorType = ParsingError String
               | UndefinedVariableError String
               | InvalidArgumentError String
               deriving (Show, Eq)

handleError :: ErrorType -> IO ()
handleError err = printError err >> exitWith (ExitFailure 84)

printError :: ErrorType -> IO ()
printError err = putStrLn $ formatError err

formatError :: ErrorType -> String
formatError (UndefinedVariableError varName) = "*** ERROR: variable " ++ varName ++ " is not bound."
formatError err = "*** ERROR: " ++ show err

stringToErrorType :: String -> ErrorHandler.ErrorType
stringToErrorType str = case str of
    "Error: The file is empty." -> ErrorHandler.ParsingError str
    _ -> ErrorHandler.InvalidArgumentError str