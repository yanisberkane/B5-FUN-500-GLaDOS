module VMTypes where
-- *VMTypes
-- $vmtypes

{- $vmtypes
    This module contains the types used in the Glados project.
-}

-- |Values for the VM
data Value = IntValue Int -- ^ Integer
            | BoolValue Bool -- ^ Boolean
            | Operator Operator -- ^ Operator
            | Function Insts -- ^ Function
            | StringValue String -- ^ String
            | ListValue [Value] -- ^ List
            deriving (Show, Eq)

-- |Operators for mathematical and logical operations
data Operator = Add -- ^ Addition
            | Sub -- ^ Subtraction
            | Mul -- ^ Multiplication
            | Div -- ^ Division
            | Mod -- ^ Modulo
            | And -- ^ And
            | Or -- ^ Or
            | Not -- ^ Not
            | Eq -- ^ Equal
            | NotEq -- ^ Not equal
            | Less -- ^ Less
            | Sup -- ^ Sup
            | Concat -- ^ Concat
            | LessEq -- ^ Less or equal
            | SupEq -- ^ Sup or equal
            deriving (Show, Eq)

-- |Instructions for the VM
data Instruction = Push Value -- ^ Push value to stack
                 | Call Int -- ^ Call function with number of arguments
                 | CallOp -- ^ Call operator for mathematical and logical operations
                 | Ret  -- ^ Return from function
                 | JumpIfFalse Int -- ^ Jump to instruction at index if top of stack is false
                 | JumpIfTrue Int -- ^ Jump to instruction at index if top of stack is true
                 | PushArg Int -- ^ Push argument at index of arguments list to stack
                 | PushVMEnv String -- ^ Push value of variable with name to stack
                 | OperateOnList Operator -- ^ Apply instructions to list on top of stack
                 | AssignEnvValue String -- ^ Assign value on top of stack to Env variable with name
                 | PushToOutput -- ^ Push last value of stack to output
                 | Jump Int -- ^ Jump to instruction at index
                 deriving (Show, Eq)

type Stack = [Value] -- ^ Stack
type Insts = [Instruction] -- ^ Instructions
type TempInsts = [Insts] -- ^ Temporary instructions
type Args = [Value] -- ^ Arguments
type Output = [Value] -- ^ Output of the VM
type VMState = (Stack, Insts, TempInsts, [Args], Output) -- ^ State of the VM
type VMEnv = [(String, Value)] -- ^ Environment of the VM

safeIndex :: [a] -> Int -> Maybe a -- ^ Safe index of a list
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)
