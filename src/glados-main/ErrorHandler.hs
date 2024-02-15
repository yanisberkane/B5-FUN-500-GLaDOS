{-
-- EPITECH PROJECT, 2024
-- B5-FUN-500-GLaDOS-Mirror [WSL: Ubuntu]
-- File description:
-- ErrorHandler
-}
module ErrorHandler (
    -- *ErrorType
    -- $errors
    ErrorType(..),
    printError,
    formatError,
    handleError,
    stringToErrorType
) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Types (Ast(..))

{- $errors
    This module contains the error handler used in the Glados interpreter.

    ErrorType is the type used to represent the errors.
-}

data ErrorType = ParsingError String -- ^ Error during parsing
               | UndefinedVariableError String -- ^ Variable is not bound
               | InvalidArgumentError String -- ^ Invalid argument
               deriving (Show, Eq)

handleError :: ErrorType -> IO () -- ^ Handle an error
handleError err = printError err >> exitWith (ExitFailure 84)

printError :: ErrorType -> IO () -- ^ Print an error
printError err = putStrLn $ formatError err

formatError :: ErrorType -> String -- ^ Format an error
formatError (UndefinedVariableError varName) =
    "*** ERROR: variable " ++ varName ++ " is not bound."
formatError err = "*** ERROR: " ++ show err

stringToErrorType :: String -> ErrorHandler.ErrorType -- ^ Convert a string to an error type
stringToErrorType str = case str of
    "Error: The file is empty." -> ErrorHandler.ParsingError str
    _ -> ErrorHandler.InvalidArgumentError str