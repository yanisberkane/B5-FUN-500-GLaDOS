module Operators where

import Types (Ast(..), Env)
import ErrorHandler (ErrorType(..))

type BuiltInFunction = Env -> [Ast] -> Either ErrorType (Ast, Env)

add :: BuiltInFunction
add env args = arithmeticOp env args sum

subtract :: BuiltInFunction
subtract env [x] = Left $ RuntimeError "Subtraction requires at least two arguments"
subtract env (x:xs) = arithmeticOp env (x:xs) (\ns -> foldl (-) (head ns) (tail ns))

multiply :: BuiltInFunction
multiply env args = arithmeticOp env args product

divide :: BuiltInFunction
divide env [x] = Left $ RuntimeError "Division requires at least two arguments"
divide env (x:xs) = arithmeticOp env (x:xs) (\ns -> foldl div (head ns) (tail ns))

arithmeticOp :: Env -> [Ast] -> ([Int] -> Int) -> Either ErrorType (Ast, Env)
arithmeticOp env args op = do
    evaluatedArgs <- mapM (\ast -> astToInt ast >>= \n -> return (AstInt n, env)) args
    let ints = map (\(ast, _) -> astToInt ast) evaluatedArgs
    case sequence ints of
        Right ns -> Right (AstInt $ op ns, env)
        Left err -> Left err

astToInt :: Ast -> Either ErrorType Int
astToInt (AstInt n) = Right n
astToInt _ = Left $ TypeError "Expected an integer"