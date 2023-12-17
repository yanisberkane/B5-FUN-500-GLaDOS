module Interpreter where

import Types (Ast(..), Env)
import qualified Data.Map as Map
import qualified Operators as Ops
import Debug.Trace (trace)
import ErrorHandler (ErrorType(..))

type BuiltInFunction = Env -> [Ast] -> Either ErrorType (Ast, Env)

evaluate :: Env -> Ast -> Either ErrorType (Ast, Env)
evaluate env ast =
    let debugMessage = "Debug: Entering evaluate with AST - " ++ show ast
    in trace debugMessage $ case ast of
        AstString str -> Right (AstString str, env)
        AstList [AstSymbol "define", nameAst@(AstSymbol _), valueExpr] ->
            evaluateDefine env nameAst valueExpr
        AstInt n -> Right (AstInt n, env)
        AstBool b -> Right (AstBool b, env)
        AstSymbol s -> evaluateSymbol env s
        If cond thenExpr elseExpr ->
            evaluateIf env cond thenExpr elseExpr
        AstList lst -> evaluateList env lst
        _ -> Left $ RuntimeError "Unhandled AST type"

evaluateDefine :: Env -> Ast -> Ast -> Either ErrorType (Ast, Env)
evaluateDefine env (AstSymbol name) valueExpr =
    let debugMessage = "Debug: Entering evaluateDefine"
    in trace debugMessage $ case evaluate env valueExpr of
        Right (val, _) -> Right (Define (AstSymbol name) val val, Map.insert name val env)
        Left err -> Left err

evaluateSymbol :: Env -> String -> Either ErrorType (Ast, Env)
evaluateSymbol env name =
    let debugMessage = "Debug: Entering evaluateSymbol with symbol " ++ name
    in trace debugMessage $ case Map.lookup name env of
        Just val -> Right (val, env)
        Nothing -> Left $ UndefinedVariableError ("Undefined symbol: " ++ name)

evaluateIf :: Env -> Ast -> Ast -> Ast -> Either ErrorType (Ast, Env)
evaluateIf env cond thenExpr elseExpr =
    let debugMessage = "Debug: Entering evaluateIf"
    in trace debugMessage $ do
        (evaluatedCond, _) <- evaluate env cond
        case evaluatedCond of
            AstBool True ->
                let trueBranchMsg = "Debug: True branch"
                in trace trueBranchMsg $ evaluate env thenExpr
            AstBool False ->
                let falseBranchMsg = "Debug: False branch"
                in trace falseBranchMsg $ evaluate env elseExpr
            _ -> Left $ RuntimeError "Condition in if statement is not a boolean"

evaluateList :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateList env (AstSymbol func : args) =
    let debugMessage = "Debug: Entering evaluateList with function " ++ func
    in trace debugMessage $ case getBuiltInFunction func of
        Just operation -> operation env args
        Nothing -> case Map.lookup func env of
            Just (Lambda params body) -> applyLambda env params body args
            _ -> Left $ RuntimeError "Trying to call a non-function"
evaluateList env _ = Left $ RuntimeError "Invalid function call"

applyLambda :: Env -> [String] -> Ast -> [Ast] -> Either ErrorType (Ast, Env)
applyLambda env params body args =
    let debugMessage = "Debug: Entering applyLambda"
    in trace debugMessage $ if length params /= length args
       then Left $ RuntimeError "Incorrect number of arguments"
       else do
           argValues <- mapM (evaluate env) args
           let newEnv = foldl (\accEnv (param, (argVal, _)) -> Map.insert param argVal accEnv) env (zip params argValues)
           evaluate newEnv body

getBuiltInFunction :: String -> Maybe BuiltInFunction
getBuiltInFunction name = case name of
    "+" -> Just Ops.add
    "-" -> Just Ops.subtract
    "*" -> Just Ops.multiply
    "div" -> Just Ops.divide
    _ -> Nothing

symbolToString :: Ast -> String
symbolToString (AstSymbol s) = s
symbolToString _ = error "Expected a symbol"
