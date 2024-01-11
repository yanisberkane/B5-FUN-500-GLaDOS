module VMTypes where

data Value = IntValue Int
            | BoolValue Bool
            | Operator Operator
            | Function Insts
            -- | SymbolValue String
            -- | CharValue Char
            | StringValue String
            | ListValue [Value]
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
            | Sup
            | Concat
            deriving (Show, Eq)

data Instruction = Push Value
                 | Call Int
                 | CallOp
                 | Ret
                 | JumpIfFalse Int -- Jump to instruction at index if top of stack is false
                 | PushArg Int -- Push argument at index of arguments list to stack
                 | PushVMEnv String -- Push value of variable with name to stack
                 | OperateOnList Operator -- Apply instructions to list on top of stack
                 | AssignEnvValue String -- Assign value on top of stack to Env variable with name
                 | PushToOutput -- Push last value of stack to output
                 deriving (Show, Eq)

type Stack = [Value]
type Insts = [Instruction]
type TempInsts = [Insts]
type Args = [Value]
type Output = [Value]
type VMState = (Stack, Insts, TempInsts, [Args], Output)
type VMEnv = [(String, Value)]

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)
