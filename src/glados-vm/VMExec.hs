module VMExec where

import VMTypes (Value(..), Operator(..), Instruction(..), Stack, Insts, Args, VMState, VMEnv, safeIndex)

execute :: Args -> VMEnv -> VMState -> Either String VMState
execute args env (value : stack, PushToOutput : insts, tmpInsts, oldArgs, output) = execute args env (stack, insts, tmpInsts, oldArgs, output ++ [value])
execute args env (ListValue xs : stack, OperateOnList operator : insts, tmpInsts, oldArgs, output) =
    case executeListOperation operator xs of
        Left err -> Left err
        Right newValue -> execute args env (newValue : stack, insts, tmpInsts, oldArgs, output)
execute args env (stack, PushVMEnv name : insts, tmpInsts, oldArgs, output) = case lookup name env of
    Just (Function insts') -> execute args env (Function insts' : stack, insts, tmpInsts, oldArgs, output)
    Just value -> execute args env (value : stack, insts, tmpInsts, oldArgs, output)
    Nothing -> Left $ "Error: Variable " ++ name ++ " not found"
execute args env (stack, AssignEnvValue name : insts, tmpInsts, oldArgs, output) = case stack of
    (value : stack') -> execute args ((name, value) : env) (stack', insts, tmpInsts, oldArgs, output)
    _ -> Left "Error: AssignEnvValue needs a value on top of the stack"
execute args env (Function funcInsts : stack, Call nb : insts, tmpInsts, oldArgs, output) = 
    if length stack < nb
    then Left $ "Error: Call needs " ++ show nb ++ " arguments"
    else 
        let
            (args', stack') = splitAt nb stack
        in
            execute args' env (stack', funcInsts, insts : tmpInsts, args : oldArgs, output)
execute args env (stack, CallOp : insts, tmpInsts, oldArgs, output) = case stack of
    (Operator operator : stack') -> case executeOperation operator stack' of
        Left err -> Left err
        Right newStack -> execute args env (newStack, insts, tmpInsts, oldArgs, output)
    _ -> Left "Error: CallOp needs an operator on top of the stack"
execute args env (stack, Push value : insts, tmpInsts, oldArgs, output) = execute args env (value : stack, insts, tmpInsts, oldArgs, output)
execute args env (stack, Jump n : insts, tmpInsts, oldArgs, output) = execute args env (stack, drop n insts, tmpInsts, oldArgs, output)
execute args env (BoolValue False : stack, JumpIfFalse n : insts, tmpInsts, oldArgs, output) = execute args env (stack, drop n insts, tmpInsts, oldArgs, output)
execute args env (BoolValue True : stack, JumpIfFalse _ : insts, tmpInsts, oldArgs, output) = execute args env (stack, insts, tmpInsts, oldArgs, output)
execute args env (_, JumpIfFalse _ : _, _, _, _) = Left "Error: JumpIfFalse needs a boolean on top of the stack"
execute args env (stack, PushArg i : insts, tmpInsts, oldArgs, output) = case safeIndex args i of
    Just arg -> execute args env (arg : stack, insts, tmpInsts, oldArgs, output)
    Nothing -> Left $ "Error: Argument index " ++ show i ++ " out of bounds"
execute args env (stack, Ret : _, newInsts : tmpRest, currentOldArgs : oldArgs, output) = execute currentOldArgs env (stack, newInsts, tmpRest, oldArgs, output)
execute args env (stack, [], insts : tmpRest, currentOldArgs : oldArgs, output) = execute currentOldArgs env (stack, insts, tmpRest, oldArgs, output)
execute args env (stack, Ret : _, [], oldArgs, output) = Right (stack, [], [], [], output)
execute args env state@(stack, insts, tmpInsts, oldArgs, output) = Right state

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
executeOperation Less (IntValue a : IntValue b : stack) = Right (BoolValue (a > b) : stack)
executeOperation Less _ = Left "Error: Less needs two arguments of the same type"
executeOperation Sup (IntValue a : IntValue b : stack) = Right (BoolValue (a < b) : stack)
executeOperation Sup _ = Left "Error: Sup needs two arguments of the same type"
executeOperation Concat (StringValue a : StringValue b : stack) = Right (StringValue (a ++ b) : stack)
executeOperation Concat _ = Left "Error: Concat needs two arguments"
executeOperation LessEq (IntValue a : IntValue b : stack) = Right (BoolValue (a >= b) : stack)
executeOperation LessEq _ = Left "Error: LessEq needs two arguments of the same type"
executeOperation SupEq (IntValue a : IntValue b : stack) = Right (BoolValue (a <= b) : stack)
executeOperation SupEq _ = Left "Error: SupEq needs two arguments of the same type"

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
