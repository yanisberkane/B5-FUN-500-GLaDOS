module ErrorHandler (ErrorType(..), printError, formatError, formatResult, handleError) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Types (Ast(..))

data ErrorType = ParsingError String
               | RuntimeError String
               | TypeError String
               | UndefinedVariableError String
               | UndefinedFunctionError String
               | FileError String
               | InvalidInputError String
               deriving (Show, Eq)

handleError :: ErrorType -> IO ()
handleError err = printError err >> exitWith (ExitFailure 84)

printError :: ErrorType -> IO ()
printError err = putStrLn $ formatError err

formatError :: ErrorType -> String
formatError (UndefinedVariableError varName) = "*** ERROR: variable " ++ varName ++ " is not bound."
formatError err = "*** ERROR: " ++ show err

formatResult :: Ast -> Maybe String
formatResult (AstInt n) = Just $ show n
formatResult (AstBool b) = Just $ show b
formatResult (AstString s) = Just s
formatResult _ = Nothing
