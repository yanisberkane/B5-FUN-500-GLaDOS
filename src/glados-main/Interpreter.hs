module Interpreter (
    -- *Interpreter
    -- $interpreter
    interpretAST
) where

import Types
import VMTypes
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import ErrorHandler (handleError, stringToErrorType, ErrorType(..))

{- $interpreter
    This module contains the interpreter used in the Glados project.
-}

interpretAST :: [Ast] -> Either ErrorHandler.ErrorType (VMEnv, Insts) -- ^ Interpret a list of ASTs
interpretAST [] = Left (stringToErrorType "Error: The file is empty. x_x")
interpretAST astList = Right $ let (env, insts) = foldl (flip processAst) ([], []) astList
                               in (env, insts ++ [Ret])

processAst :: Ast -> (VMEnv, Insts) -> (VMEnv, Insts) -- ^ Process an AST
processAst (Define (AstSymbol sym) value) (env, insts) =
    let valueInsts = interpretMathOpOrValue [] value
    in (env, insts ++ valueInsts ++ [AssignEnvValue sym])
processAst (Assign (AstSymbol sym) value) (env, insts) =
    let valueInsts = interpretMathOpOrValue [] value
    in (env, insts ++ valueInsts ++ [AssignEnvValue sym])
processAst (AstCall (AstSymbol "print") (AstList args)) (env, insts) =
    let argsInsts = concatMap (interpretMathOpOrValue []) args
    in (env, insts ++ argsInsts ++ [PushToOutput])
processAst (AstCall (AstSymbol "return") (AstList [returnValue])) (env, insts) =
    let returnInsts = interpretMathOpOrValue [] returnValue
    in (env, insts ++ returnInsts)
processAst (AstCall (AstSymbol fName) (AstList args)) (env, insts) =
    let argsInsts = concatMap (interpretMathOpOrValue []) (reverse args)
    in (env, insts ++ argsInsts ++ [PushVMEnv fName, Call (length args)])
processAst (NamedCall (AstSymbol fName) (Lambda (AstList params) (AstList body))) (env, insts) =
    let paramNames = map (\(AstSymbol s) -> s) params
        bodyInsts = concatMap (\ast -> snd (processAst ast (env, []))) body
        funcInsts = replaceSymbolsWithArgs paramNames bodyInsts ++ [Ret]
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

replaceSymbolsWithArgs :: [String] -> Insts -> Insts -- ^ Replace symbols with arguments
replaceSymbolsWithArgs params insts = map replace insts
  where
    replace (PushVMEnv sym) | sym `elem` params = PushArg (fromJust $ elemIndex sym params)
    replace inst = inst

interpretCondition :: Ast -> Insts -- ^ Interpret a condition
interpretCondition (AstList exprs) = interpretComplexCondition exprs
interpretCondition _ = error "Invalid condition"

interpretComplexCondition :: [Ast] -> Insts -- ^ Interpret a complex condition
interpretComplexCondition exprs = processConditions exprs [] []

processConditions :: [Ast] -> [Operator] -> Insts -> Insts -- ^ Process conditions
processConditions [] ops insts = insts ++ concatMap (\op -> [Push (Operator op), CallOp]) ops
processConditions (LogicOperator op : rest) ops insts =
    let newOp = logicStringToOperator op
    in if newOp `elem` [And, Or, Not]
       then processConditions rest (ops ++ [newOp]) insts
       else processConditions rest ops insts
processConditions (left : LogicOperator op : right : rest) ops insts =
    let newOp = logicStringToOperator op
    in if newOp `elem` [And, Or, Not]
       then processConditions rest (ops ++ [newOp]) (insts ++ interpretMathOpOrValue [] left ++ interpretMathOpOrValue [] right ++ [Push (Operator newOp), CallOp])
       else processConditions rest ops (insts ++ interpretMathOpOrValue [] left ++ interpretMathOpOrValue [] right ++ [Push (Operator newOp), CallOp])
processConditions exprs _ _ = error $ "Unexpected pattern: " ++ show exprs

logicStringToOperator :: String -> Operator -- ^ Convert a string to a logic operator
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

interpretMathOpOrValue :: [String] -> Ast -> Insts -- ^ Interpret a math operation or a value
interpretMathOpOrValue params (AstMathOp left op right) =
    let rightInsts = interpretMathOpOrValue params right
        leftInsts = interpretMathOpOrValue params left
        opInst = [Push (Operator (stringToOperator (astOperatorToString op))), CallOp]
    in case op of
        AstOperator "-" -> rightInsts ++ leftInsts ++ opInst
        AstOperator "/" -> rightInsts ++ leftInsts ++ opInst
        AstOperator "%" -> rightInsts ++ leftInsts ++ opInst
        _ -> leftInsts ++ rightInsts ++ opInst
interpretMathOpOrValue params (AstSymbol sym) =
    if sym `elem` params
    then [PushArg (fromJust $ elemIndex sym params)]
    else [PushVMEnv sym]
interpretMathOpOrValue params (AstCall (AstSymbol fName) (AstList args)) =
    let argsInsts = concatMap (interpretMathOpOrValue params) args
    in argsInsts ++ [PushVMEnv fName, Call (length args)]
interpretMathOpOrValue params (AstList lst) = concatMap (interpretMathOpOrValue params) lst
interpretMathOpOrValue _ (AstInt i) = [Push (IntValue i)]
interpretMathOpOrValue _ (AstBool b) = [Push (BoolValue b)]
interpretMathOpOrValue _ (AstString s) = [Push (StringValue s)]
interpretMathOpOrValue _ ast = error $ "Invalid value for AST node: " ++ show ast

astOperatorToString :: Ast -> String -- ^ Convert an Ast operator to a string
astOperatorToString (AstOperator op) = op
astOperatorToString _ = error "Expected an operator"

stringToOperator :: String -> Operator -- ^ Convert a string to an operator
stringToOperator "+" = Add
stringToOperator "-" = Sub
stringToOperator "*" = Mul
stringToOperator "/" = Div
stringToOperator "%" = Mod
stringToOperator _ = error "Unknown operator"

interpretValue :: Ast -> Value -- ^ Interpret a value
interpretValue (AstInt i) = IntValue i
interpretValue (AstBool b) = BoolValue b
interpretValue (AstString s) = StringValue s
interpretValue (AstSymbol sym) = error $ "Unexpected symbol in direct value context: " ++ sym
interpretValue ast = error $ "Invalid value for AST node: " ++ show ast