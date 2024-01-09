module VMBinaryFileProcess where

import Data.Binary (Binary, put, get, encodeFile, decodeFile, Get)
import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, VMEnv, safeIndex)

instance Binary Value where
    put (IntValue i) = put (0 :: Int) >> put i
    put (BoolValue b) = put (1 :: Int) >> put b
    put (Operator o) = put (2 :: Int) >> put o
    put (Function insts) = put (3 :: Int) >> put insts
    put (StringValue s) = put (4 :: Int) >> put s
    put (ListValue l) = put (5 :: Int) >> put l
    get = (get :: Get Int) >>= \tag -> case tag of
        0 -> get >>= \i -> return $ IntValue i
        1 -> get >>= \b -> return $ BoolValue b
        2 -> get >>= \o -> return $ Operator o
        3 -> get >>= \insts -> return $ Function insts
        4 -> get >>= \s -> return $ StringValue s
        5 -> get >>= \l -> return $ ListValue l

instance Binary Operator where
    put Add = put (0 :: Int)
    put Sub = put (1 :: Int)
    put Mul = put (2 :: Int)
    put Div = put (3 :: Int)
    put Mod = put (4 :: Int)
    put And = put (5 :: Int)
    put Or = put (6 :: Int)
    put Not = put (7 :: Int)
    put Eq = put (8 :: Int)
    put Less = put (9 :: Int)
    put Concat = put (10 :: Int)
    get = (get :: Get Int) >>= \tag -> case tag of
        0 -> return Add
        1 -> return Sub
        2 -> return Mul
        3 -> return Div
        4 -> return Mod
        5 -> return And
        6 -> return Or
        7 -> return Not
        8 -> return Eq
        9 -> return Less
        10 -> return Concat

instance Binary Instruction where
    put (Push v) = put (0 :: Int) >> put v
    put (Call i) = put (1 :: Int) >> put i
    put Ret = put (2 :: Int)
    put (JumpIfFalse i) = put (3 :: Int) >> put i
    put (PushArg i) = put (4 :: Int) >> put i
    put (PushVMEnv s) = put (5 :: Int) >> put s
    put (OperateOnList o) = put (6 :: Int) >> put o
    put (AssignEnvValue s) = put (7 :: Int) >> put s
    get = (get :: Get Int) >>= \tag -> case tag of
        0 -> get >>= \v -> return $ Push v
        1 -> get >>= \i -> return $ Call i
        2 -> return Ret
        3 -> get >>= \i -> return $ JumpIfFalse i
        4 -> get >>= \i -> return $ PushArg i
        5 -> get >>= \s -> return $ PushVMEnv s
        6 -> get >>= \o -> return $ OperateOnList o
        7 -> get >>= \s -> return $ AssignEnvValue s

writeStateToFile :: FilePath -> ([(String, Value)], Insts) -> IO ()
writeStateToFile filename envinsts =  encodeFile filename envinsts

readStateFromFile :: FilePath -> IO ([(String, Value)], Insts)
readStateFromFile filename = decodeFile filename
