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

astToInt :: Ast -> Either ErrorType Int
astToInt (AstInt n) = Right n
astToInt _ = Left $ TypeError "Expected an integer"