module Operators where

import Types (Ast(..), Env)
import ErrorHandler (ErrorType(..))

type EvaluateFunc = Env -> Ast -> Either ErrorType (Ast, Env)

add :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
add evaluate env args = arithmeticOp evaluate env args sum

subtract :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
subtract evaluate env [x] = Left $ RuntimeError "Subtraction requires at least two arguments"
subtract evaluate env (x:xs) = arithmeticOp evaluate env (x:xs) (\ns -> foldl (-) (head ns) (tail ns))

multiply :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
multiply evaluate env args = arithmeticOp evaluate env args product

divide :: EvaluateFunc -> Env -> [Ast] -> Either ErrorType (Ast, Env)
divide evaluate env [x] = Left $ RuntimeError "Division requires at least two arguments"
divide evaluate env (x:xs) = arithmeticOp evaluate env (x:xs) (\ns -> foldl div (head ns) (tail ns))

arithmeticOp :: EvaluateFunc -> Env -> [Ast] -> ([Int] -> Int) -> Either ErrorType (Ast, Env)
arithmeticOp evaluate env args op = do
    evaluatedArgs <- mapM (evaluate env) args
    let ints = mapM (\(ast, _) -> astToInt ast) evaluatedArgs
    case ints of
        Right ns -> Right (AstInt $ op ns, env)
        Left err -> Left err

astToInt :: Ast -> Either ErrorType Int
astToInt (AstInt n) = Right n
astToInt _ = Left $ TypeError "Expected an integer"