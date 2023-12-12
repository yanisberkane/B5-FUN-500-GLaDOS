module ErrorHandler (handleError, ErrorType(..)) where
import System.Exit (exitWith, ExitCode(ExitFailure))

data ErrorType = ParsingError String
               | RuntimeError String
               | TypeError String
               | UndefinedVariableError String
               | UndefinedFunctionError String
               | FileError String
               deriving (Show, Eq)

handleError :: ErrorType -> IO ()
handleError error = do
    putStrLn $ "Error: " ++ show error
    exitWith (ExitFailure 84)
