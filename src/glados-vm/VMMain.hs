
import System.IO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, VMEnv, safeIndex)
import VMExec (execute)
import VMBinaryFileProcess (writeStateToFile, readStateFromFile)

main :: IO ()
main = do
    args <- getArgs
    let env = [("absCode", (Function [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret]))]
    let insts = [PushVMEnv "absCode", Call, Ret]
    writeStateToFile "file.exec" (env, insts)

    (venv, vinsts) <- readStateFromFile "file.exec"

    putStrLn $ show venv
    putStrLn $ show vinsts

    let res = execute [(IntValue (-42))] venv ([], vinsts)
    putStrLn $ show res
    return ()
