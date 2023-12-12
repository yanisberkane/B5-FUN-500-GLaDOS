import Test.HUnit
import Interpreter (evaluate, sExprToAst)
import Types (Ast(..), SExpr(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))

testDefineVariable :: Test
testDefineVariable = TestCase $ do
    let env = Map.empty
    let ast = sExprToAst (SList [SSymbol "define", SSymbol "feo", SInt 21])
    let expected = Right (Define (SSymbol "foo") (SInt 21) (AstInt 21), Map.fromList [("foo", AstInt 21)])
    assertEqual "Define variable foo to 21" expected (evaluate env ast)

tests :: Test
tests = TestList [ TestLabel "testDefineVariable" testDefineVariable
                 ]

main :: IO Counts
main = runTestTT tests
