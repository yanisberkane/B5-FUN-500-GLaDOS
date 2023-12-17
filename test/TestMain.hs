import Test.HUnit
import Interpreter (evaluate)
import Types (Ast(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))

evaluateAst :: Env -> Maybe Ast -> Either ErrorType (Ast, Env)
evaluateAst env maybeAst = case maybeAst of
    Just ast -> evaluate env ast
    Nothing -> Left $ RuntimeError "Failed to parse AST"

testDivision :: Test
testDivision = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "div", AstInt 10, AstInt 2])
    let expected = Right (AstInt 5, env)
    assertEqual "div 10 2" expected (evaluateAst env ast)

testDefineAndUseVariable :: Test
testDefineAndUseVariable = TestCase $ do
    let initialEnv = Map.empty
    let defineAst = AstList [AstSymbol "define", AstSymbol "foo", AstInt 21]
    let defineResult = evaluateAst initialEnv (Just defineAst)
    let expectedEnv = Map.fromList [("foo", AstInt 21)]
    let expected = Right (Define (AstSymbol "foo") (AstInt 21) (AstInt 21), expectedEnv)

    assertEqual "define foo 21" expected defineResult

testUndefinedVariableError :: Test
testUndefinedVariableError = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "*", AstSymbol "foo", AstInt 2])
    let expected = Left $ UndefinedVariableError "Undefined symbol: foo"
    assertEqual "(* foo 2) with undefined foo" expected (evaluateAst env ast)

tests :: Test
tests = TestList [
                   TestLabel "testDivision" testDivision,
                   TestLabel "testDefineAndUseVariable" testDefineAndUseVariable,
                   TestLabel "testUndefinedVariableError" testUndefinedVariableError
                 ]

main :: IO Counts
main = runTestTT tests
