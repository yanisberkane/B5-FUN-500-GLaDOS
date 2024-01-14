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
processAst (AstCall (AstSymbol "return") (AstList [returnValue])) (env, insts) =
    let returnInsts = interpretMathOpOrValue returnValue
    in (env, insts ++ returnInsts)
processAst (AstCall (AstSymbol fName) (AstList args)) (env, insts) =
    let argsInsts = concatMap interpretMathOpOrValue args
    in (env, insts ++ argsInsts ++ [PushVMEnv fName, Call (length args)])
processAst (NamedCall (AstSymbol fName) (Lambda (AstList params) (AstList body))) (env, insts) =
    let paramNames = map (\(AstSymbol s) -> s) params
        bodyInsts = concatMap (\ast -> snd (processAst ast (env, insts))) body
        funcInsts = concat (zipWith (\name idx -> [PushArg idx, AssignEnvValue name]) paramNames [0..]) ++ bodyInsts ++ [Ret]
    in ((fName, Function funcInsts):env, insts)
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
interpretCondition (AstList exprs) = interpretComplexCondition exprs
interpretCondition _ = error "Invalid condition"

interpretComplexCondition :: [Ast] -> Insts
interpretComplexCondition exprs = processConditions exprs [] []

processConditions :: [Ast] -> [Operator] -> Insts -> Insts
processConditions [] ops insts = insts ++ concatMap (\op -> [Push (Operator op), CallOp]) ops
processConditions (LogicOperator op : rest) ops insts =
    let newOp = logicStringToOperator op
    in if newOp `elem` [And, Or, Not]
       then processConditions rest (ops ++ [newOp]) insts
       else processConditions rest ops insts
processConditions (left : LogicOperator op : right : rest) ops insts =
    let newOp = logicStringToOperator op
    in if newOp `elem` [And, Or, Not]
       then processConditions rest (ops ++ [newOp]) (insts ++ interpretMathOpOrValue left ++ interpretMathOpOrValue right ++ [Push (Operator newOp), CallOp])
       else processConditions rest ops (insts ++ interpretMathOpOrValue left ++ interpretMathOpOrValue right ++ [Push (Operator newOp), CallOp])
processConditions exprs _ _ = error $ "Unexpected pattern: " ++ show exprs

logicStringToOperator :: String -> Operator
logicStringToOperator "==" = Eq
logicStringToOperator "<"  = Less
logicStringToOperator ">"  = Sup
logicStringToOperator "<=" = LessEq
logicStringToOperator ">=" = SupEq
logicStringToOperator "&&" = And
logicStringToOperator "||" = Or
logicStringToOperator "!=" = NotEq
logicStringToOperator "!"  = Not
logicStringToOperator _ = error "Unknown logic operator"

interpretMathOpOrValue :: Ast -> Insts
interpretMathOpOrValue (AstMathOp left op right) =
    let leftInsts = interpretMathOpOrValue left
        rightInsts = interpretMathOpOrValue right
        opInst = [Push (Operator (stringToOperator (astOperatorToString op))), CallOp]
    in leftInsts ++ rightInsts ++ opInst
interpretMathOpOrValue (AstSymbol sym) = [PushVMEnv sym]
interpretMathOpOrValue (AstCall (AstSymbol fName) (AstList args)) =
    let argsInsts = concatMap interpretMathOpOrValue args
    in argsInsts ++ [PushVMEnv fName, Call (length args)]
interpretMathOpOrValue (AstList lst) = concatMap interpretMathOpOrValue lst
interpretMathOpOrValue ast = [Push (interpretValue ast)]

astOperatorToString :: Ast -> String
astOperatorToString (AstOperator op) = op
astOperatorToString _ = error "Expected an operator"

stringToOperator :: String -> Operator
stringToOperator "+" = Add
stringToOperator "-" = Sub
stringToOperator "*" = Mul
stringToOperator "/" = Div
stringToOperator _ = error "Unknown operator"

interpretValue :: Ast -> Value
interpretValue (AstInt i) = IntValue i
interpretValue (AstBool b) = BoolValue b
interpretValue (AstString s) = StringValue s
interpretValue (AstSymbol sym) = error $ "Unexpected symbol in direct value context: " ++ sym
interpretValue _ = error "Invalid value"