module ErrorHandler (ErrorType(..), printError, formatError, handleError) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Types (Ast(..))

data ErrorType = ParsingError String
               | UndefinedVariableError String
               | InvalidArgumentError String
               | InvalidConditionError Ast
               | InvalidOperatorError String
               | InvalidFunctionError String
               deriving (Show, Eq)

handleError :: ErrorType -> IO ()
handleError err = printError err >> exitWith (ExitFailure 84)

printError :: ErrorType -> IO ()
printError err = putStrLn $ formatError err

formatError :: ErrorType -> String
formatError (UndefinedVariableError varName) = "*** ERROR: variable " ++ varName ++ " is not bound."
formatError err = "*** ERROR: " ++ show err