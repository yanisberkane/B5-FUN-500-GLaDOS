module Interpreter where

import Types (Ast(..), SExpr(..), Env)
import qualified Data.Map as Map
import qualified Operators as Ops
import ErrorHandler (ErrorType(..))

evaluate :: Env -> Ast -> Either ErrorType (Ast, Env)
evaluate env ast = case ast of
    AstList [AstSymbol "define", AstSymbol name, valueExpr] ->
        evaluateDefine env name valueExpr
    AstInt n -> Right (AstInt n, env)
    AstSymbol s -> evaluateSymbol env s
    AstList lst -> evaluateList env lst
    _ -> Left $ RuntimeError "Unhandled AST type"

evaluateDefine :: Env -> String -> Ast -> Either ErrorType (Ast, Env)
evaluateDefine env name valueAst =
    case evaluate env valueAst of
        Right (val, newEnv) -> Right (Define (SSymbol name) (astToSExpr val) val, Map.insert name val newEnv)
        Left err -> Left err

evaluateSymbol :: Env -> String -> Either ErrorType (Ast, Env)
evaluateSymbol env name =
    case Map.lookup name env of
        Just val -> Right (val, env)
        Nothing -> Left $ UndefinedVariableError ("Undefined symbol: " ++ name)

evaluateList :: Env -> [Ast] -> Either ErrorType (Ast, Env)
evaluateList env (AstSymbol func : args) = case func of
    "+" -> Ops.add evaluate env args
    "-" -> Ops.subtract evaluate env args
    _ -> Left $ UndefinedFunctionError ("Unknown function: " ++ func)
evaluateList env _ = Right (AstInt 0, env)


sExprToAst :: SExpr -> Ast
sExprToAst (SInt n) = AstInt n
sExprToAst (SSymbol s) = AstSymbol s
sExprToAst (SList lst) = AstList $ map sExprToAst lst

astToSExpr :: Ast -> SExpr
astToSExpr (AstInt n) = SInt n
astToSExpr (AstSymbol s) = SSymbol s
astToSExpr (AstList lst) = SList $ map astToSExpr lst