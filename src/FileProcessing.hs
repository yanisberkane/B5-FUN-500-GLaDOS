module FileProcessing (processFile) where
import Control.Monad
import ErrorHandler
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.FilePath (takeExtension)

processFile :: FilePath -> IO ()
processFile filename
    | takeExtension filename /= ".glados" = handleError $ FileError "Invalid file extension. Expecting '.glados'"
    | otherwise = readFile filename `catchIOError` handleFileReadError >>= processContent

handleFileReadError :: IOError -> IO String
handleFileReadError e
    | isDoesNotExistError e = handleError (FileError "File does not exist.") >> return ""
    | otherwise = handleError (FileError $ "Error reading file: " ++ show e) >> return ""

processContent :: String -> IO ()
processContent content
    | null content = handleError $ FileError "File is empty."
    | otherwise = do
        putStrLn "PASING GOES HERE !!! - File content processed:"
        print content