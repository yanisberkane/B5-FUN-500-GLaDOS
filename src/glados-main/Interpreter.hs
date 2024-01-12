module Interpreter (interpretAST) where

import Types
import VMTypes

interpretAST :: [Ast] -> (VMEnv, Insts)
interpretAST astList = let (env, insts) = foldl (flip processAst) ([], []) astList
                       in (env, insts ++ [Ret])

processAst :: Ast -> (VMEnv, Insts) -> (VMEnv, Insts)
processAst (Define (AstSymbol sym) value) (env, insts) =
    let valueInsts = interpretMathOpOrValue value
    in (env, insts ++ valueInsts ++ [AssignEnvValue sym])
processAst (Assign (AstSymbol sym) value) (env, insts) =
    let valueInsts = interpretMathOpOrValue value
    in (env, insts ++ valueInsts ++ [AssignEnvValue sym])
processAst (AstCall (AstSymbol "print") (AstList args)) (env, insts) =
    let argsInsts = concatMap interpretMathOpOrValue args
    in (env, insts ++ argsInsts ++ [PushToOutput])
processAst (If cond thenBranch elseBranch) (env, insts) =
    let condInsts = interpretCondition cond
        processBranch (AstList stmts) = concatMap (snd . (`processAst` (env, []))) stmts
        processBranch stmt = snd $ processAst stmt (env, [])
        thenInsts = processBranch thenBranch
        elseInsts = processBranch elseBranch
        jumpOverThen = [JumpIfFalse (length thenInsts + if null elseInsts then 0 else 1)]
        skipElseInst = if null elseInsts then [] else [Jump (length elseInsts)]
    in (env, insts ++ condInsts ++ jumpOverThen ++ thenInsts ++ skipElseInst ++ elseInsts)
processAst _ (env, insts) = (env, insts)

interpretCondition :: Ast -> Insts
interpretCondition (AstList [left, LogicOperator op, right]) =
    case op of
        "<=" -> interpretCompositeOp left right ["<", "=="]
        ">=" -> interpretCompositeOp left right [">", "=="]
        _    -> interpretMathOpOrValue left ++ interpretMathOpOrValue right ++ [Push (Operator (logicStringToOperator op)), CallOp]
interpretCondition _ = error "Invalid condition"

interpretCompositeOp :: Ast -> Ast -> [String] -> Insts
interpretCompositeOp left right ops =
    let leftInsts = interpretMathOpOrValue left
        rightInsts = interpretMathOpOrValue right
        opInsts = concatMap (\op -> leftInsts ++ rightInsts ++ [Push (Operator (logicStringToOperator op)), CallOp]) ops
    in opInsts ++ [Push (Operator Or), CallOp]

logicStringToOperator :: String -> Operator
logicStringToOperator "==" = Eq
logicStringToOperator "<"  = Less
logicStringToOperator ">"  = Sup
logicStringToOperator "&&" = And
logicStringToOperator "||" = Or
logicStringToOperator _ = error "Unknown logic operator"

interpretMathOpOrValue :: Ast -> Insts
interpretMathOpOrValue (AstMathOp left op right) = interpretMathOp left op right
interpretMathOpOrValue (AstSymbol sym) = [PushVMEnv sym]
interpretMathOpOrValue (AstList lst) = concatMap interpretMathOpOrValue lst
interpretMathOpOrValue ast = [Push (interpretValue ast)]

interpretMathOp :: Ast -> Ast -> Ast -> Insts
interpretMathOp left (AstOperator op) right =
    interpretMathOpOrValue left ++ interpretMathOpOrValue right ++ [Push (Operator (stringToOperator op)), CallOp]
interpretMathOp _ _ _ = error "Invalid MathOp structure"

interpretValue :: Ast -> Value
interpretValue (AstInt i) = IntValue i
interpretValue (AstBool b) = BoolValue b
interpretValue (AstString s) = StringValue s
interpretValue (AstSymbol sym) = error $ "Unexpected symbol in direct value context: " ++ sym
interpretValue _ = error "Invalid value"

stringToOperator :: String -> Operator
stringToOperator "+" = Add
stringToOperator "-" = Sub
stringToOperator "*" = Mul
stringToOperator "/" = Div
stringToOperator _ = error "Unknown operator"