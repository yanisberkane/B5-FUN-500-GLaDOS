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

tests :: Test
tests = TestList [ TestLabel "testDefineVariable" testDefineVariable
                 , TestLabel "testUndefinedVariable" testUndefinedVariable
                 ]

main :: IO Counts
main = runTestTT tests