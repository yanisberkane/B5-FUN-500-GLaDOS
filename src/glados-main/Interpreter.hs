module Interpreter where

import Types
import VMTypes (VMEnv, Insts, Value(..), Operator(..), Instruction(..))

interpretAST :: [Ast] -> (VMEnv, Insts)
interpretAST astList = foldr processAst ([], []) astList

processAst :: Ast -> (VMEnv, Insts) -> (VMEnv, Insts)
processAst ast (env, insts) = case ast of
    Define (AstSymbol sym) value -> ((sym, interpretValue value) : env, insts)
    -- Assign (AstSymbol sym) value -> (env, insts ++ [PushVMEnv sym, Push (interpretValue value), Call])
    If cond thenExpr elseExpr -> ifInsts cond thenExpr elseExpr (env, insts)
    Lambda params body -> (env, insts ++ [Push (Function (lambdaInsts params body))])
    -- NamedCall (AstSymbol sym) args -> (env, insts ++ [PushVMEnv sym, Push (interpretValue args), Call])
    -- AstCall func args -> (env, insts ++ [Push (interpretValue func), Push (interpretValue args), Call])
    _ -> (env, insts)

interpretValue :: Ast -> Value
interpretValue (AstInt i) = IntValue i
interpretValue (AstBool b) = BoolValue b
interpretValue (AstString s) = StringValue s
interpretValue (AstList lst) = ListValue $ map interpretValue lst

ifInsts :: Ast -> Ast -> Ast -> (VMEnv, Insts) -> (VMEnv, Insts)
ifInsts cond thenExpr elseExpr (env, insts) =
    let
        (envCond, instsCond) = processAst cond (env, [])
        (envThen, instsThen) = processAstList thenExpr (envCond, [])
        (envElse, instsElse) = processAstList elseExpr (envThen, [])
        offsetElse = length instsThen + 1
    in
        (envElse, insts ++ instsCond ++ [JumpIfFalse offsetElse] ++ instsThen ++ instsElse)

processAstList :: Ast -> (VMEnv, Insts) -> (VMEnv, Insts)
processAstList (AstList asts) (env, insts) = foldr processAst (env, insts) asts
processAstList ast (env, insts) = processAst ast (env, insts)

lambdaInsts :: Ast -> Ast -> Insts
lambdaInsts params body = [PushArg 0]