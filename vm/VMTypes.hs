module VMTypes where

data Value = IntValue Int
            | BoolValue Bool
            | Operator Operator
            | Function Insts
            -- | UIntValue Int
            -- | SymbolValue String
            -- | CharValue Char
            -- | StringValue String
            -- | ListValue [Value]
            deriving (Show, Eq)

data Operator = Add
            | Sub
            | Mul
            | Div
            | Mod
            | And
            | Or
            | Not
            | Eq
            | Less
            deriving (Show, Eq)

data Instruction = Push Value
                 | Call
                 | Ret
                 | JumpIfFalse Int -- Jump to instruction at index if top of stack is false
                 | PushArg Int -- Push argument at index of arguments list to stack
                 | PushEnv String -- Push value of variable with name to stack
                 deriving (Show, Eq)

type Stack = [Value]
type Insts = [Instruction]
type Args = [Value]
type VMState = (Stack, Insts)
type Env = [(String, Value)]

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)
