
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
        [filename] | takeExtension filename == ".dz" -> processFile filename Nothing
                   | otherwise -> handleError $ ParsingError "Invalid file extension. Expected '.dz' >:C."
        [filename, "-d"] | takeExtension filename == ".dz" -> processFile filename (Just "-d")
                   | otherwise -> handleError $ ParsingError "Invalid file extension. Expected '.dz' >:C."
        _ -> handleError $ InvalidArgumentError "File was not provided."

debug :: Either String VMState -> IO ()
debug (Left err) = putStrLn $ "Error: " ++ err
debug (Right (vstack, vinsts, vtempInsts, vtempArgs, voutput)) = 
    putStrLn $ "\nDebug Mode:\nStack: " ++ show vstack ++ "\n" ++
               "Instructions: " ++ show vinsts ++ "\n" ++
               "Temp instructions: " ++ show vtempInsts ++ "\n" ++
               "Temp arguments: " ++ show vtempArgs ++ "\n" ++
               "Output: " ++ show voutput

processFile :: FilePath -> Maybe String -> IO ()
processFile filename debugFlag = do
    (venv, vinsts) <- readStateFromFile filename
    let res = execute [] venv ([], vinsts, [], [], [])
    case res of
        Left err -> putStrLn $ "Error: " ++ err
        Right (_, _, _, _, output) -> putStr $ formatStack output
    case debugFlag of
        Just "-d" -> debug res
        Nothing -> return ()

formatStack :: Stack -> String
formatStack [] = ""
formatStack (IntValue i:xs) = show i ++ "\n" ++ formatStack xs
formatStack (BoolValue b:xs) = show b ++ "\n" ++ formatStack xs
formatStack (Operator o:xs) = show o ++ "\n" ++ formatStack xs
formatStack (Function insts:xs) = show insts ++ "\n" ++ formatStack xs
formatStack (StringValue s:xs) = "\"" ++ s ++ "\"" ++ "\n" ++ formatStack xs
formatStack (ListValue l:xs) = show l ++ "\n" ++ formatStack xs

handleReadError :: IOException -> IO ()
handleReadError e = handleError $ InvalidArgumentError ("File error: " ++ show e)
