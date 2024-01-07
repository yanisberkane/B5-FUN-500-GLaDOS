module VMErrorHandler (VMErrorType(..), printError, formatError, handleError) where

import System.Exit (exitWith, ExitCode(ExitFailure))

data VMErrorType = ParsingError String
               | RuntimeError String
               | TypeError String
               | UndefinedVariableError String
               | UndefinedFunctionError String
               | FileError String
               | InvalidInputError String
               deriving (Show, Eq)

handleError :: VMErrorType -> IO ()
handleError err = printError err >> exitWith (ExitFailure 84)

printError :: VMErrorType -> IO ()
printError err = putStrLn $ formatError err

formatError :: VMErrorType -> String
formatError (UndefinedVariableError varName) = "*** ERROR: variable " ++ varName ++ " is not bound."
formatError err = "*** ERROR: " ++ show err
