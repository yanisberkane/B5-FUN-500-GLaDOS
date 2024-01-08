
import System.FilePath (takeExtension)
import System.IO
import System.Environment
import Control.Monad
import VMErrorHandler (formatError, handleError, VMErrorType(..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Exit (exitFailure)
import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, VMEnv, safeIndex)
import qualified Data.Map as Map

-- processContent :: String -> IO ()
-- processContent content
--     | null content = handleError $ FileError "No input provided."
--     | otherwise = maybe (handleError $ ParsingError "Failed to parse the content.") (processExpressions Map.empty) (stringToSExpr content)

processFile :: FilePath -> IO ()
processFile filename
    | takeExtension filename /= ".exec" = handleError $ FileError "Invalid file extension. Expecting '.exec'"
    | otherwise = readFile filename `catchIOError` handleFileReadError >>= putStrLn

handleFileReadError :: IOError -> IO String
handleFileReadError e = do
    putStrLn $ formatError $ if isDoesNotExistError e
        then FileError "File does not exist."
        else FileError $ "Error reading file: " ++ show e
    exitFailure
    return ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Error: Invalid arguments"
    return ()
