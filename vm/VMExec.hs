module VMExec where

import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, Env, safeIndex)

execute :: Args -> Env -> VMState -> Either String VMState
execute args env (stack, PushEnv name : insts) = case lookup name env of
    Just (Function insts') -> execute args env (stack, insts' ++ insts)
    Just value -> execute args env (value : stack, insts)
    Nothing -> Left $ "Error: Variable " ++ name ++ " not found"
execute args env (stack, Call : insts) = case stack of
    (Operator operator : stack') -> case executeOperation operator stack' of
        Left err -> Left err
        Right newStack -> execute args env (newStack, insts)
    (Function functionInsts : stack') -> execute args env (stack', functionInsts ++ insts)
    _ -> Left "Error: Call needs an operator or a function on top of the stack"
execute args env (stack, Push value : insts) = execute args env (value : stack, insts)
execute args env (stack, Ret : _) = Right (stack, [])
execute args env (BoolValue False : stack, JumpIfFalse n : insts) = execute args env (stack, drop n insts)
execute args env (BoolValue True : stack, JumpIfFalse _ : insts) = execute args env (stack, insts)
execute args env (_, JumpIfFalse _ : _) = Left "Error: JumpIfFalse needs a boolean on top of the stack"
execute args env (stack, PushArg i : insts) = case safeIndex args i of
    Just arg -> execute args env (arg : stack, insts)
    Nothing -> Left $ "Error: Argument index " ++ show i ++ " out of bounds"
execute _ _ state@(_, []) = Right state

executeOperation :: Operator -> Stack -> Either String Stack
executeOperation Add (IntValue a : IntValue b : stack) = Right (IntValue (a + b) : stack)
executeOperation Add _ = Left "Error: Add needs two arguments"
executeOperation Sub (IntValue a : IntValue b : stack) = Right (IntValue (a - b) : stack)
executeOperation Sub _ = Left "Error: Sub needs two arguments"
executeOperation Mul (IntValue a : IntValue b : stack) = Right (IntValue (a * b) : stack)
executeOperation Mul _ = Left "Error: Mul needs two arguments"
executeOperation Div (IntValue a : IntValue 0 : stack) = Left "Error: division by 0"
executeOperation Div (IntValue a : IntValue b : stack) = Right (IntValue (a `div` b) : stack)
executeOperation Div _ = Left "Error: Div needs two arguments"
executeOperation Mod (IntValue a : IntValue 0 : stack) = Left "Error: division by 0"
executeOperation Mod (IntValue a : IntValue b : stack) = Right (IntValue (a `mod` b) : stack)
executeOperation Mod _ = Left "Error: Mod needs two arguments"
executeOperation And (BoolValue a : BoolValue b : stack) = Right (BoolValue (a && b) : stack)
executeOperation And _ = Left "Error: And needs two arguments"
executeOperation Or (BoolValue a : BoolValue b : stack) = Right (BoolValue (a || b) : stack)
executeOperation Or _ = Left "Error: Or needs two arguments"
executeOperation Not (BoolValue a : stack) = Right (BoolValue (not a) : stack)
executeOperation Not _ = Left "Error: Not needs one boolean argument"
executeOperation Eq (IntValue a : IntValue b : stack) = Right (BoolValue (a == b) : stack)
executeOperation Eq (BoolValue a : BoolValue b : stack) = Right (BoolValue (a == b) : stack)
executeOperation Eq _ = Left "Error: Eq needs two arguments of the same type"
executeOperation Less (IntValue a : IntValue b : stack) = Right (BoolValue (a < b) : stack)
executeOperation Less _ = Left "Error: Less needs two arguments of the same type"
