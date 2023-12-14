module Operators where

import Types (Ast(..), Env)
import ErrorHandler (ErrorType(..))

type EvaluateFunc = Env -> Ast -> Either ErrorType (Ast, Env)

add :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
add evaluate env args = do
    evaluatedArgs <- mapM (evaluate env) args
    let ints = mapM (\(ast, _) -> astToInt ast) evaluatedArgs
    case ints of
        Right ns -> Right (AstInt $ sum ns, env)
        Left err -> Left err

subtract :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
subtract evaluate env (x:xs) = do
    evaluatedArgs <- mapM (evaluate env) (x:xs)
    let ints = mapM (\(ast, _) -> astToInt ast) evaluatedArgs
    case ints of
        Right (n:ns) -> Right (AstInt $ foldl (-) n ns, env)
        Right [] -> Left $ RuntimeError "Subtraction needs at least one argument"
        Left err -> Left err

multiply :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
multiply evaluate env args = do
    evaluatedArgs <- mapM (evaluate env) args
    let ints = mapM (\(ast, _) -> astToInt ast) evaluatedArgs
    case ints of
        Right ns -> Right (AstInt $ product ns, env)
        Left err -> Left err

divide :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
divide evaluate env (x:xs) = do
    evaluatedArgs <- mapM (evaluate env) (x:xs)
    let ints = mapM (\(ast, _) -> astToInt ast) evaluatedArgs
    case ints of
        Right (n:ns) -> Right (AstInt $ foldl (div) n ns, env)
        Right [] -> Left $ RuntimeError "Division needs at least one argument"
        Left err -> Left err

astToInt :: Ast -> Ei ther ErrorType Int
astToInt (AstInt n) = Right n
astToInt _ = Left $ TypeError "Expected an integer"