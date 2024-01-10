module VMExec where

import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, VMEnv, safeIndex)

pushToArgs :: Value -> Args -> Args
pushToArgs value args = value : args

execute :: Args -> VMEnv -> VMState -> Either String VMState
execute args env (ListValue xs : stack, OperateOnList operator : insts) =
    case executeListOperation operator xs of
        Left err -> Left err
        Right newValue -> execute args env (newValue : stack, insts)
execute args env (stack, PushVMEnv name : insts) = case lookup name env of
    Just (Function insts') -> execute args env (Function insts' : stack, insts)
    Just value -> execute args env (value : stack, insts)
    Nothing -> Left $ "Error: Variable " ++ name ++ " not found"
execute args env (stack, AssignEnvValue name : insts) = case stack of
    (value : stack') -> execute args ((name, value) : env) (stack', insts)
    _ -> Left "Error: AssignEnvValue needs a value on top of the stack"
execute args env (Function funcInsts : stack, Call nb : insts) = 
    if length stack < nb
    then Left $ "Error: Call needs " ++ show nb ++ " arguments"
    else 
        let
            (args', stack') = splitAt nb stack
            newArgs = foldr pushToArgs [] args'
        in
            execute newArgs env (stack', funcInsts ++ insts)
execute args env (stack, CallOp : insts) = case stack of
    (Operator operator : stack') -> case executeOperation operator stack' of
        Left err -> Left err
        Right newStack -> execute args env (newStack, insts)
    _ -> Left "Error: CallOp needs an operator on top of the stack"
execute args env (stack, Push value : insts) = execute args env (value : stack, insts)
execute args env (stack, Ret : _) = Right (stack, [])
execute args env (BoolValue False : stack, JumpIfFalse n : insts) = execute args env (stack, drop n insts)
execute args env (BoolValue True : stack, JumpIfFalse _ : insts) = execute args env (stack, insts)
execute args env (_, JumpIfFalse _ : _) = Left "Error: JumpIfFalse needs a boolean on top of the stack"
execute args env (stack, PushArg i : insts) = case safeIndex args i of
    Just arg -> execute args env (arg : stack, insts)
    Nothing -> Left $ "Error: Argument index " ++ show i ++ " out of bounds"
execute args _ state@(_, []) = Right state

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
executeOperation Sup (IntValue a : IntValue b : stack) = Right (BoolValue (a > b) : stack)
executeOperation Sup _ = Left "Error: Sup needs two arguments of the same type"
executeOperation Concat (StringValue a : StringValue b : stack) = Right (StringValue (a ++ b) : stack)
executeOperation Concat _ = Left "Error: Concat needs two arguments"

executeListOperation :: Operator -> [Value] -> Either String Value
executeListOperation Add xs = Right $ IntValue $ sum [x | IntValue x <- xs]
executeListOperation Sub (x:xs) = Right $ IntValue $ foldl (-) (toInt x) [toInt x' | x' <- xs]
    where toInt (IntValue x) = x
executeListOperation Sub _ = Left "Error: Sub needs at least one argument"
executeListOperation Mul xs = Right $ IntValue $ product [x | IntValue x <- xs]
executeListOperation Div (x:xs) = Right $ IntValue $ foldl (div) (toInt x) [toInt x' | x' <- xs]
    where toInt (IntValue x) = x
executeListOperation Div _ = Left "Error: Div needs at least one argument"
executeListOperation Mod (x:xs) = Right $ IntValue $ foldl (mod) (toInt x) [toInt x' | x' <- xs]
    where toInt (IntValue x) = x
executeListOperation Mod _ = Left "Error: Mod needs at least one argument"
executeListOperation And xs = Right $ BoolValue $ and [x | BoolValue x <- xs]
executeListOperation Or xs = Right $ BoolValue $ or [x | BoolValue x <- xs]
executeListOperation Not (BoolValue x:_) = Right $ BoolValue $ not x
executeListOperation Not _ = Left "Error: Not needs one boolean argument"
executeListOperation Eq (x:xs) = Right $ BoolValue $ and [x == x' | x' <- xs]
executeListOperation Eq _ = Left "Error: Eq needs at least one argument"
executeListOperation Less (IntValue x : xs) = Right $ BoolValue $ and [x < x' | IntValue x' <- xs]
executeListOperation Less _ = Left "Error: Less needs at least one argument"
