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

testLambda1 :: Test
testLambda1 = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "lambda", AstList [AstSymbol "a", AstSymbol "b"], AstList [AstSymbol "+", AstSymbol "a", AstSymbol "b"]])
    let expected = Right (AstString "#<procedure>", env)
    assertEqual "(lambda (a b) (+ a b))" expected (evaluateAst env ast)

testLambda2 :: Test
testLambda2 = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstList [AstSymbol "lambda", AstList [AstSymbol "a", AstSymbol "b"], AstList [AstSymbol "+", AstSymbol "a", AstSymbol "b"]], AstInt 1, AstInt 2])
    let expectedEnv = Map.fromList [("a", AstInt 1), ("b", AstInt 2)]
    let expected = Right (AstInt 3, expectedEnv)
    assertEqual "((lambda (a b) (+ a b)) 1 2)" expected (evaluateAst env ast)

testIf1 :: Test
testIf1 = TestCase $ do
    let env = Map.empty
    let ast = Just (If (AstBool True) (AstInt 1) (AstInt 2))
    let expected = Right (AstInt 1, env)
    assertEqual "(if #t 1 2)" expected (evaluateAst env ast)

testIf2 :: Test
testIf2 = TestCase $ do
    let env = Map.empty
    let ast = Just (If (AstBool False) (AstInt 1) (AstInt 2))
    let expected = Right (AstInt 2, env)
    assertEqual "(if #f 1 2)" expected (evaluateAst env ast)

testIf3 :: Test
testIf3 = TestCase $ do
    let initialEnv = Map.empty
    let defineAst = AstList [AstSymbol "define", AstSymbol "foo", AstInt 42]
    let defineResult = evaluateAst initialEnv (Just defineAst)
    let expectedEnv = Map.fromList [("foo", AstInt 42)]
    let ifAst = If (AstList [AstSymbol "<", AstSymbol "foo", AstInt 10]) (AstList [AstSymbol "*", AstSymbol "foo", AstInt 3]) (AstList [AstSymbol "div", AstSymbol "foo", AstInt 2])
    let ifResult = evaluateAst expectedEnv (Just ifAst)
    let expected = Right (AstInt 21, expectedEnv)
    assertEqual "(if (< foo 10) (* foo 3) (div foo 2))" expected ifResult

testBuiltins1 :: Test
testBuiltins1 = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "+", AstList [AstSymbol "*", AstInt 2, AstInt 3], AstList [AstSymbol "div", AstInt 10, AstInt 2]])
    let expected = Right (AstInt 11, env)
    assertEqual "(+ (* 2 3) (div 10 2))" expected (evaluateAst env ast)

testBuiltins2 :: Test
testBuiltins2 = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "eq?", AstList [AstSymbol "*", AstInt 2, AstInt 5], AstList [AstSymbol "-", AstInt 11, AstInt 1]])
    let expected = Right (AstBool True, env)
    assertEqual "(eq? (* 2 5) (- 11 1))" expected (evaluateAst env ast)

testBasicAddition :: Test
testBasicAddition = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "+", AstInt 1, AstInt 2])
    let expected = Right (AstInt 3, env)
    assertEqual "(+ 1 2)" expected (evaluateAst env ast)

testBasicSubtraction :: Test
testBasicSubtraction = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "-", AstInt 5, AstInt 2])
    let expected = Right (AstInt 3, env)
    assertEqual "(- 5 2)" expected (evaluateAst env ast)

testIfWithAddition :: Test
testIfWithAddition = TestCase $ do
    let env = Map.empty
    let ast = Just (If (AstBool True) (AstList [AstSymbol "+", AstInt 1, AstInt 2]) (AstInt 2))
    let expected = Right (AstInt 3, env)
    assertEqual "(if #t (+ 1 2) 2)" expected (evaluateAst env ast)

testIfWithSubtraction :: Test
testIfWithSubtraction = TestCase $ do
    let env = Map.empty
    let ast = Just (If (AstBool False) (AstInt 1) (AstList [AstSymbol "-", AstInt 5, AstInt 2]))
    let expected = Right (AstInt 3, env)
    assertEqual "(if #f 1 (- 5 2))" expected (evaluateAst env ast)

testModulo :: Test
testModulo = TestCase $ do
    let env = Map.empty
    let ast = Just (AstList [AstSymbol "mod", AstInt 10, AstInt 3])
    let expected = Right (AstInt 1, env)
    assertEqual "(mod 10 3)" expected (evaluateAst env ast)

tests :: Test
tests = TestList [
                   TestLabel "testDivision" testDivision,
                   TestLabel "testBasicAddition" testBasicAddition,
                   TestLabel "testBasicSubtraction" testBasicSubtraction,
                   TestLabel "testDefineAndUseVariable" testDefineAndUseVariable,
                   TestLabel "testUndefinedVariableError" testUndefinedVariableError,
                   TestLabel "testLambda1" testLambda1,
                   TestLabel "testLambda2" testLambda2,
                   TestLabel "testIf1" testIf1,
                   TestLabel "testIf2" testIf2,
                   TestLabel "testIf3" testIf3,
                   TestLabel "testBuiltins1" testBuiltins1,
                   TestLabel "testBuiltins2" testBuiltins2,
                   TestLabel "testIfWithAddition" testIfWithAddition,
                   TestLabel "testIfWithSubtraction" testIfWithSubtraction,
                   TestLabel "testModulo" testModulo
                 ]

main :: IO Counts
main = runTestTT tests