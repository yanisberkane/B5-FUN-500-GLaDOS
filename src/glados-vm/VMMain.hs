
import System.IO
import System.Environment (getArgs)
import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, VMEnv, safeIndex)
import VMExec (execute)
import VMBinaryFileProcess (readStateFromFile)
import System.FilePath (takeExtension, (<.>))
import Control.Exception (IOException, try)
import ErrorHandler (handleError, ErrorType(ParsingError, InvalidArgumentError))

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filename] | takeExtension filename == ".dz" -> processFile filename
                   | otherwise -> handleError $ ParsingError "Invalid file extension. Expected '.dz' >:C."
        _ -> handleError $ InvalidArgumentError "File was not provided."

processFile :: FilePath -> IO ()
processFile filename = do
    (venv, vinsts) <- readStateFromFile filename
    let res = execute [] venv ([], vinsts, [], [], [])
    case res of
        Left err -> putStrLn $ "Error: " ++ err
        Right (_, _, _, _, output) -> putStrLn $ formatStack output

formatStack :: Stack -> String
formatStack [] = ""
formatStack (IntValue i:xs) = show i ++ formatStack xs
formatStack (BoolValue b:xs) = show b ++ formatStack xs
formatStack (Operator o:xs) = show o ++ formatStack xs
formatStack (Function insts:xs) = show insts ++ formatStack xs
formatStack (StringValue s:xs) = "\"" ++ s ++ "\"" ++ formatStack xs
formatStack (ListValue l:xs) = show l ++ formatStack xs

handleReadError :: IOException -> IO ()
handleReadError e = handleError $ InvalidArgumentError ("File error: " ++ show e)
