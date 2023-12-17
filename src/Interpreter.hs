module Interpreter where

import Types (Ast(..), Env)
import qualified Data.Map as Map
import qualified Operators as Ops
import ErrorHandler (ErrorType(..))

evaluate :: Env -> Ast -> Either ErrorType (Ast, Env)
evaluate env ast = case ast of
    AstString str -> Right (AstString str, env)
    AstList [AstSymbol "define", nameAst@(AstSymbol _), valueExpr] ->
        evaluateDefine env nameAst valueExpr
    AstInt n -> Right (AstInt n, env)
    AstBool b -> Right (AstBool b, env)
    AstSymbol s -> evaluateSymbol env s
    AstList [AstSymbol "lambda", AstList params, body] ->
        Right (AstString "#<procedure>", env)
    AstList (AstList [AstSymbol "lambda", AstList params, body]:args) ->
        let paramNames = map symbolToString params
        in applyLambda env paramNames body args
    If cond thenExpr elseExpr ->
        evaluateIf env cond thenExpr elseExpr
    AstList lst -> evaluateList env lst
    _ -> Left $ RuntimeError "Unhandled AST type"

evaluateDefine :: Env -> Ast -> Ast -> Either ErrorType (Ast, Env)
evaluateDefine env (AstSymbol name) valueExpr = case evaluate env valueExpr of
    Right (val, _) -> Right (Define (AstSymbol name) val val, Map.insert name val env)
    Left err -> Left err

evaluateSymbol :: Env -> String -> Either ErrorType (Ast, Env)
evaluateSymbol env name = case Map.lookup name env of
    Just val -> Right (val, env)
    Nothing -> Left $ UndefinedVariableError ("Undefined symbol: " ++ name)

evaluateIf :: Env -> Ast -> Ast -> Ast -> Either ErrorType (Ast, Env)
evaluateIf env cond thenExpr elseExpr = do
    (evaluatedCond, _) <- evaluate env cond
    case evaluatedCond of
        AstBool True -> evaluate env thenExpr
        AstBool False -> evaluate env elseExpr
        _ -> Left $ RuntimeError "Condition in if statement is not a boolean"

evaluateList :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateList env (firstElement:args) = do
    evaluatedArgs <- mapM (evaluate env) args
    let argValues = map fst evaluatedArgs
    let newEnv = snd $ head evaluatedArgs

    case firstElement of
        AstSymbol func ->
            case func of
                "+" -> Ops.add newEnv argValues
                "-" -> Ops.subtract newEnv argValues
                "*" -> Ops.multiply newEnv argValues
                "div" -> Ops.divide newEnv argValues
                "mod" -> modulo newEnv argValues
                "<" -> evaluateLessThan newEnv argValues
                ">" -> evaluateGreaterThan newEnv argValues
                "eq?" -> evaluateEqual newEnv argValues
                _ -> case Map.lookup func env of
                    Just (Lambda params body) -> applyLambda newEnv params body argValues
                    _ -> Left $ RuntimeError "Trying to call a non-function or undefined operator"
        AstList [AstSymbol "lambda", AstList params, body] ->
            let paramNames = map symbolToString params
            in applyLambda newEnv paramNames body argValues
        _ -> Left $ RuntimeError "Invalid function call format"
evaluateList _ _ = Left $ RuntimeError "Invalid function call"

modulo :: Env -> [Ast] -> Either ErrorType (Ast, Env)
modulo env [AstInt x, AstInt y] = Right (AstInt (x `mod` y), env)
modulo _ _ = Left $ TypeError "Invalid arguments for mod operator"

evaluateEqual :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateEqual env [x, y] = do
    evaluatedX <- evaluate env x
    evaluatedY <- evaluate env y
    case (evaluatedX, evaluatedY) of
        ((AstInt intX, _), (AstInt intY, _)) -> Right (AstBool (intX == intY), env)
        _ -> Left $ TypeError "Invalid arguments for eq? operator"

evaluateGreaterThan :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateGreaterThan env [x, y] = do
    evaluatedX <- evaluate env x
    evaluatedY <- evaluate env y
    case (evaluatedX, evaluatedY) of
        ((AstInt intX, _), (AstInt intY, _)) -> Right (AstBool (intX > intY), env)
        _ -> Left $ TypeError "Invalid arguments for > operator"

evaluateLessThan :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateLessThan env [x, y] = do
    evaluatedX <- evaluate env x
    evaluatedY <- evaluate env y
    case (evaluatedX, evaluatedY) of
        ((AstInt intX, _), (AstInt intY, _)) -> Right (AstBool (intX < intY), env)
        _ -> Left $ TypeError "Invalid arguments for < operator"

applyLambda :: Env -> [String] -> Ast -> [Ast] -> Either ErrorType (Ast, Env)
applyLambda env params body args =
    if length params /= length args
    then Left $ RuntimeError "Incorrect number of arguments"
    else do
        argValues <- mapM (evaluate env) args
        let argVals = map fst argValues
        let newEnv = foldl (\accEnv (param, val) -> Map.insert param val accEnv) env (zip params argVals)
        evaluate newEnv body

symbolToString :: Ast -> String
symbolToString (AstSymbol s) = s
symbolToString _ = error "Expected a symbol"