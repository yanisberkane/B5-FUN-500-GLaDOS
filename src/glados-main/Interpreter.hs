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
processAst _ (env, insts) = (env, insts)

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