import Test.HUnit
import Interpreter (evaluate, sExprToAst)
import Types (Ast(..), SExpr(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))

testDefineVariable :: Test
testDefineVariable = TestCase $ do
    let env = Map.empty
    let ast = sExprToAst (SList [SSymbol "define", SSymbol "foo", SInt 42])
    let expected = Right (Define (AstSymbol "foo") (AstInt 42) (AstInt 42), Map.fromList [("foo", AstInt 42)])
    assertEqual "Define variable foo to 42" expected (evaluate env ast)

testUndefinedVariable :: Test
testUndefinedVariable = TestCase $ do
    let env = Map.empty
    let ast = sExprToAst (SSymbol "foo")
    let expected = Left $ UndefinedVariableError "Undefined symbol: foo"
    assertEqual "Undefined variable foo" expected (evaluate env ast)

testLambdaExpression :: Test
testLambdaExpression = TestCase $ do
    let env = Map.empty
    let ast = sExprToAst (SList [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]])
    case evaluate env ast of
      Left err -> assertFailure $ "Lambda expression failed with error: " ++ show err
      Right _ -> assertBool "Lambda expression evaluated" True

testLambdaFunctionCall :: Test
testLambdaFunctionCall = TestCase $ do
    let env = Map.empty
    let ast = sExprToAst (SList [SList [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]], SInt 1, SInt 2])
    assertEqual "Lambda function call (1 + 2)" (Right (AstInt 3, env)) (evaluate env ast)

testDefinedFunctionCall :: Test
testDefinedFunctionCall = TestCase $ do
    let initialEnv = Map.empty
    let defineAst = sExprToAst (SList [SSymbol "define", SList [SSymbol "add", SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]])
    let (_, env) = case evaluate initialEnv defineAst of
                      Right res -> res
                      Left err -> error $ "Error defining function: " ++ show err
    let callAst = sExprToAst (SList [SSymbol "add", SInt 3, SInt 4])
    assertEqual "Defined function call (3 + 4)" (Right (AstInt 7, env)) (evaluate env callAst)

tests :: Test
tests = TestList [ TestLabel "testDefineVariable" testDefineVariable
                 , TestLabel "testUndefinedVariable" testUndefinedVariable
                 , TestLabel "testLambdaExpression" testLambdaExpression
                 , TestLabel "testLambdaFunctionCall" testLambdaFunctionCall
                 , TestLabel "testDefinedFunctionCall" testDefinedFunctionCall
                 ]

main :: IO Counts
main = runTestTT tests