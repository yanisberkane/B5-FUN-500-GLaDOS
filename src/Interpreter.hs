module Interpreter where

import Types (Ast(..), SExpr(..), Env)
import qualified Data.Map as Map
import qualified Operators as Ops
import ErrorHandler (ErrorType(..))

evaluate :: Env -> Ast -> Either ErrorType (Ast, Env)
evaluate env ast = case ast of
    AstList [AstSymbol "define", nameAst@(AstSymbol _), valueExpr] ->
        evaluateDefine env nameAst valueExpr
    AstInt n -> Right (AstInt n, env)
    AstSymbol s -> evaluateSymbol env s
    AstList lst -> evaluateList env lst
    _ -> Left $ RuntimeError "Unhandled AST type"

evaluateDefine :: Env -> Ast -> Ast -> Either ErrorType (Ast, Env)
evaluateDefine env (AstSymbol name) valueExpr =
    case evaluate env valueExpr of
        Right (val, _) -> Right (Define (AstSymbol name) val val, Map.insert name val env)
        Left err -> Left err
evaluateDefine env (AstList (AstSymbol name : params)) body =
    let paramNames = map symbolToString params
        lambda = Lambda paramNames body
    in Right (Define (AstSymbol name) lambda lambda, Map.insert name lambda env)
evaluateDefine _ _ _ = Left $ RuntimeError "Invalid define syntax"

evaluateSymbol :: Env -> String -> Either ErrorType (Ast, Env)
evaluateSymbol env name =
    case Map.lookup name env of
        Just val -> Right (val, env)
        Nothing -> Left $ UndefinedVariableError ("Undefined symbol: " ++ name)

evaluateList :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateList env (AstSymbol func : args) =
    case func of
        "+" -> Ops.add evaluate env args
        "-" -> Ops.subtract evaluate env args
        "*" -> Ops.multiply evaluate env args
        "div" -> Ops.divide evaluate env args
        "lambda" -> case args of
            [AstList params, body] -> Right (Lambda (map symbolToString params) body, env)
            _ -> Left $ RuntimeError "Invalid lambda expression"
        _ -> evaluateFunctionCall env (AstSymbol func : args)

evaluateFunctionCall :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateFunctionCall env (functionAst:args) = do
    func <- evaluate env functionAst
    case func of
        (Lambda params body, closureEnv) -> applyLambda closureEnv params body args
        _ -> Left $ RuntimeError "Trying to call a non-function"

applyLambda :: Env -> [String] -> Ast -> [Ast] -> Either ErrorType (Ast, Env)
applyLambda env params body args =
    if length params /= length args then
        Left $ RuntimeError "Incorrect number of arguments"
    else do
        argValues <- mapM (evaluate env) args
        let newEnv = foldl (\acc (name, val) -> Map.insert name val acc) env (zip params (map fst argValues))
        evaluate newEnv body

symbolToString :: Ast -> String
symbolToString (AstSymbol s) = s
symbolToString _ = error "Expected a symbol"

sExprToAst :: SExpr -> Ast
sExprToAst (SInt n) = AstInt n
sExprToAst (SSymbol "#t") = AstBool True
sExprToAst (SSymbol "#f") = AstBool False
sExprToAst (SSymbol s) = AstSymbol s
sExprToAst (SList lst) = AstList $ map sExprToAst lst

astToSExpr :: Ast -> SExpr
astToSExpr (AstInt n) = SInt n
astToSExpr (AstBool b) = SSymbol (if b then "#t" else "#f")
astToSExpr (AstSymbol s) = SSymbol s
astToSExpr (AstList lst) = SList $ map astToSExpr lst
