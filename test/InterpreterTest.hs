module InterpreterTest (runTests) where
import Interpreter
import Types
import ErrorHandler (ErrorType(..))
import qualified Data.Map as Map

formatResult :: Either ErrorType (Ast, Env) -> String
formatResult (Left err) = formatError err
formatResult (Right (ast, _)) = formatAst ast

formatError :: ErrorType -> String
formatError (UndefinedVariableError varName) = "*** ERROR: variable " ++ varName ++ " is not bound."
formatError err = "*** ERROR: " ++ show err

formatAst :: Ast -> String
formatAst (AstInt n) = show n
formatAst (AstSymbol s) = s
formatAst (Define (SSymbol name) _ _) = "'" ++ name ++ "' defined"
formatAst _ = "not haeZndled"

runTests :: IO ()
runTests = do
    putStrLn "--Running Interpreter Tests--"
    let initialEnv = Map.empty
    let testExpr1 = AstSymbol "foo"
    let testExpr2 = Define (SSymbol "unused") (SSymbol "foo") (AstInt 42)

    putStrLn $ "> foo\n" ++ formatResult (evaluate initialEnv testExpr1)
    let (result2, updatedEnv) = case evaluate initialEnv testExpr2 of
                                  Right res -> res
                                  Left err -> error $ "Error: " ++ show err
    putStrLn $ "> (define foo 42)\n" ++ formatResult (Right (result2, updatedEnv))
    let testExpr3 = AstSymbol "foo"
    putStrLn $ "> foo\n" ++ formatResult (evaluate updatedEnv testExpr3)
    putStrLn "--Interpreter Tests Completed--"
