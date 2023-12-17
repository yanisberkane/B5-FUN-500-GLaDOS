import Test.HUnit
import Interpreter (evaluate)
import Types (Ast(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))

testDefineVariable :: Test
testDefineVariable = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "define", AstSymbol "foo", AstInt 42])
    let expected = Right (Define (AstSymbol "foo") (AstInt 42) (AstInt 42), Map.insert "foo" (AstInt 42) env)
    case ast of
        Just a -> assertEqual "Define variable foo to 42" expected (evaluate env a)
        Nothing -> assertFailure "Failed to parse AST"

testUndefinedVariable :: Test
testUndefinedVariable = TestCase $ do
    let env = Map.empty
    let ast = Just (AstSymbol "foo")
    let expected = Left $ UndefinedVariableError "Undefined symbol: foo"
    case ast of
        Just a -> assertEqual "Undefined variable foo" expected (evaluate env a)
        Nothing -> assertFailure "Failed to parse AST"

tests :: Test
tests = TestList [ TestLabel "testDefineVariable" testDefineVariable
                 , TestLabel "testUndefinedVariable" testUndefinedVariable
                 ]

main :: IO Counts
main = runTestTT tests
