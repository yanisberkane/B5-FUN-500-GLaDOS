import Test.HUnit
import Interpreter (evaluate)
import Types (Ast(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))
import Parser
import Control.Applicative
import System.Exit
import VMExec (execute)
import VMTypes

-- Tests for the VM

testPushVM :: Test
testPushVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 42), Ret])
    let expected = Right ([IntValue 42],[])
    assertEqual "execute [] [] ([], [Push (IntValue 42), Ret])" expected (executed)

testAddVM :: Test
testAddVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 42), Push (IntValue 1), Push (Operator Add), Call, Ret])
    let expected = Right ([IntValue 43],[])
    assertEqual "execute [] [] ([], [Push (IntValue 42), Push (IntValue 1), Push (Operator Add), Call, Ret])" expected (executed)

testPushFailVM :: Test
testPushFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Add), Call, Ret])
    let expected = Left "Error: Add needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Add), Call, Ret])" expected (executed)

testDivFailVM :: Test
testDivFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 0), Push (IntValue 42), Push (Operator Div), Call, Ret])
    let expected = Left "Error: division by 0"
    assertEqual "execute [] [] ([], [Push (IntValue 0), Push (IntValue 42), Push (Operator Div), Call, Ret])" expected (executed)

testEqVM :: Test
testEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), Call, Ret])
    let expected = Right ([BoolValue True],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), Call, Ret])" expected (executed)

testLessVM :: Test
testLessVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 2), Push (IntValue 5), Push (Operator Less), Call, Ret])
    let expected = Right ([BoolValue False],[])
    assertEqual "execute [] [] ([], [Push (IntValue 2), Push (IntValue 5), Push (Operator Less), Call, Ret])" expected (executed)

testJumpIfFalse1VM :: Test
testJumpIfFalse1VM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), Call, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])
    let expected = Right ([IntValue 1],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), Call, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

testJumpIfFalse2Vm :: Test
testJumpIfFalse2Vm = TestCase $ do
    let executed1 = execute [(IntValue 42)] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret])
    let expected1 = Right ([IntValue 42], [])
    assertEqual "execute [(IntValue 42)] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret])" expected1 (executed1)
    let executed2 = execute [(IntValue (-42))] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret])
    let expected2 = Right ([IntValue 42],[])
    assertEqual "execute [(IntValue (-42))] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret])" expected2 (executed2)

testEnvVM :: Test
testEnvVM = TestCase $ do
    let executed = execute [(IntValue (-42))] [("absCode", (Function [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret]))] ([], [PushVMEnv "absCode", Call, Ret])
    let expected = Right ([IntValue 42],[])
    assertEqual "execute [(IntValue (-42))] [(\"absCode\", (Function [PushArg 0, Push (IntValue 0), Push (Operator Less), Call, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), Call, Ret]))] ([], [PushVMEnv \"absCode\", Call, Ret])" expected (executed)

-- Evaluate AST

evaluateAst :: Env -> Maybe Ast -> Either ErrorType (Ast, Env)
evaluateAst env maybeAst = case maybeAst of
    Just ast -> evaluate env ast
    Nothing -> Left $ RuntimeError "Failed to parse AST"

-- Tests for the parser

testAlternative :: Test
testAlternative = TestCase $ do
    let parser = runParser (parseChar 'a' <|> parseChar 'b') "abcd"
    let expected = Just ('a', "bcd")
    assertEqual "parseChar 'a' <|> parseChar 'b' on 'abcd'" expected (parser)

testEmptyAlternative :: Test
testEmptyAlternative = TestCase $ do
    let parser = runParser (parseChar 'a' <|> parseChar 'b' <|> empty) "cdef"
    let expected = Nothing
    assertEqual "parseChar 'a' <|> parseChar 'b' on 'cdef'" expected (parser)

testParseChar :: Test
testParseChar = TestCase $ do
    let parser = runParser (parseChar 'a') "abcd"
    let expected = Just ('a', "bcd")
    assertEqual "parseChar 'a' on 'abcd'" expected (parser)

testParseCharFail :: Test
testParseCharFail = TestCase $ do
    let parser = runParser (parseChar 'a') "bcde"
    let expected = Nothing
    assertEqual "parseChar 'a' on 'bcde'" expected (parser)

testParseAnyChar :: Test
testParseAnyChar = TestCase $ do
    let parser = runParser (parseAnyChar "bca") "cdef"
    let expected = Just ('c', "def")
    assertEqual "parseAnyChar \"bca\" on \"cdef\"" expected (parser)

testParseAnyCharFail :: Test
testParseAnyCharFail = TestCase $ do
    let parser = runParser (parseAnyChar "bca") "defg"
    let expected = Nothing
    assertEqual "parseAnyChar \"bca\" on \"defg\"" expected (parser)

testParseOr :: Test
testParseOr = TestCase $ do
    let parser = runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
    let expected = Just ('a', "bcd")
    assertEqual "parseOr (parseChar 'a') (parseChar 'b') on 'abcd'" expected (parser)

testParseOr2 :: Test
testParseOr2 = TestCase $ do
    let parser = runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcde"
    let expected = Just ('b', "cde")
    assertEqual "parseOr (parseChar 'a') (parseChar 'b') on 'bcde'" expected (parser)

testParseAnd :: Test
testParseAnd = TestCase $ do
    let parser = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
    let expected = Just (('a', 'b'), "cd")
    assertEqual "parseAnd (parseChar 'a') (parseChar 'b') on 'abcd'" expected (parser)

testParseAndFail :: Test
testParseAndFail = TestCase $ do
    let parser = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acde"
    let expected = Nothing
    assertEqual "parseAnd (parseChar 'a') (parseChar 'b') on 'acde'" expected (parser)

testParseAndFail2 :: Test
testParseAndFail2 = TestCase $ do
    let parser = runParser (parseAnd (parseChar 'z') (parseChar 'b')) "bcde"
    let expected = Nothing
    assertEqual "parseAnd (parseChar 'a') (parseChar 'b') on 'bcde'" expected (parser)

testParseAndWith :: Test
testParseAndWith = TestCase $ do
    let parser = runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd"
    let expected = Just ("ab", "cd")
    assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') on 'abcd'" expected (parser)

testParseAndWithFail :: Test
testParseAndWithFail = TestCase $ do
    let parser = runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "acde"
    let expected = Nothing
    assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') on 'acde'" expected (parser)

testParseAndWithFail2 :: Test
testParseAndWithFail2 = TestCase $ do
    let parser = runParser (parseAndWith (\x y -> [x, y]) (parseChar 'z') (parseChar 'b')) "bcde"
    let expected = Nothing
    assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') on 'bcde'" expected (parser)

testParseMany :: Test
testParseMany = TestCase $ do
    let parser = runParser (parseMany (parseChar 'a')) "aaabcd"
    let expected = Just ("aaa", "bcd")
    assertEqual "parseMany (parseChar 'a') on 'aaabcd'" expected (parser)

testParseManyFail :: Test
testParseManyFail = TestCase $ do
    let parser = runParser (parseMany (parseChar 'a')) "bcde"
    let expected = Just ("", "bcde")
    assertEqual "parseMany (parseChar 'a') on 'bcde'" expected (parser)

testParseManyFail2 :: Test
testParseManyFail2 = TestCase $ do
    let parser = runParser (parseMany (parseChar ' ')) ""
    let expected = Just ("", "")
    assertEqual "parseMany (parseChar ' ') on ''" expected (parser)

testParseSome :: Test
testParseSome = TestCase $ do
    let parser = runParser (parseSome (parseChar 'a')) "aaabcd"
    let expected = Just ("aaa", "bcd")
    assertEqual "parseSome (parseChar 'a') on 'aaabcd'" expected (parser)

testParseSomeFail :: Test
testParseSomeFail = TestCase $ do
    let parser = runParser (parseSome (parseChar 'a')) "bcde"
    let expected = Nothing
    assertEqual "parseSome (parseChar 'a') on 'bcde'" expected (parser)

testParseSomeFail2 :: Test
testParseSomeFail2 = TestCase $ do
    let parser = runParser (parseSome (parseChar 'z')) "testMessage"
    let expected = Nothing
    assertEqual "parseSome (parseChar 'z') on 'testMessage'" expected (parser)

testParseNoneOf :: Test
testParseNoneOf = TestCase $ do
    let parser = runParser (parseNoneOf "abc") "defg"
    let expected = Just ('d', "efg")
    assertEqual "parseNoneOf \"abc\" on \"defg\"" expected (parser)

testParseNoneOfFail :: Test
testParseNoneOfFail = TestCase $ do
    let parser = runParser (parseNoneOf "abc") "abc"
    let expected = Nothing
    assertEqual "parseNoneOf \"abc\" on \"abc\"" expected (parser)

testParseUInt :: Test
testParseUInt = TestCase $ do
    let parser = runParser parseUInt "1234"
    let expected = Just (1234, "")
    assertEqual "parseUInt on \"1234\"" expected (parser)

testParseUIntFail :: Test
testParseUIntFail = TestCase $ do
    let parser = runParser parseUInt "abc"
    let expected = Nothing
    assertEqual "parseUInt on \"abc\"" expected (parser)

testParseInt :: Test
testParseInt = TestCase $ do
    let parser = runParser parseInt "-1234"
    let expected = Just (-1234, "")
    assertEqual "parseInt on \"-1234\"" expected (parser)

testParseInt2 :: Test
testParseInt2 = TestCase $ do
    let parser = runParser parseInt "+1234"
    let expected = Just (1234, "")
    assertEqual "parseInt on \"+1234\"" expected (parser)

testParseIntFail :: Test
testParseIntFail = TestCase $ do
    let parser = runParser parseInt "abc"
    let expected = Nothing
    assertEqual "parseInt on \"abc\"" expected (parser)

testParseQuotedSymbol :: Test
testParseQuotedSymbol = TestCase $ do
    let parser = runParser parseQuotedSymbol "\"foo\""
    let expected = Just ("foo", "")
    assertEqual "parseQuotedSymbol on \"foo\"" expected (parser)

testParseQuotedSymbolFail :: Test
testParseQuotedSymbolFail = TestCase $ do
    let parser = runParser parseQuotedSymbol "foo"
    let expected = Nothing
    assertEqual "parseQuotedSymbol on \"foo\"" expected (parser)

testParseSymbol :: Test
testParseSymbol = TestCase $ do
    let parser = runParser parseSymbol "foo"
    let expected = Just ("foo", "")
    assertEqual "parseSymbol on \"foo\"" expected (parser)

testParseSymbolFail :: Test
testParseSymbolFail = TestCase $ do
    let parser = runParser parseSymbol "\"foo\""
    let expected = Nothing
    assertEqual "parseSymbol on \"foo\"" expected (parser)

testParseWhiteSpace :: Test
testParseWhiteSpace = TestCase $ do
    let parser = runParser parseWhiteSpace "  foo"
    let expected = Just ("  ", "foo")
    assertEqual "parseWhiteSpace on \"  foo\"" expected (parser)

testParseWhiteSpaceFail :: Test
testParseWhiteSpaceFail = TestCase $ do
    let parser = runParser parseWhiteSpace "foo"
    let expected = Nothing
    assertEqual "parseWhiteSpace on \"foo\"" expected (parser)

-- testParseString :: Test
-- testParseString = TestCase $ do
--     let parser = runParser parseString "\"foo\""
--     let expected = Just ("foo", "")
--     assertEqual "parseString on \"foo\"" expected (parser)

-- testParseStringFail :: Test
-- testParseStringFail = TestCase $ do
--     let parser = runParser parseString "foo"
--     let expected = Nothing
--     assertEqual "parseString on \"foo\"" expected (parser)

-- Tests for the interpreter

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
                    TestLabel "testPushVM" testPushVM,
                    TestLabel "testAddVM" testAddVM,
                    TestLabel "testPushFailVM" testPushFailVM,
                    TestLabel "testDivFailVM" testDivFailVM,
                    TestLabel "testEqVM" testEqVM,
                    TestLabel "testLessVM" testLessVM,
                    TestLabel "testJumpIfFalse1VM" testJumpIfFalse1VM,
                    TestLabel "testJumpIfFalse2Vm" testJumpIfFalse2Vm,
                    TestLabel "testEnvVM" testEnvVM,
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
                    TestLabel "testModulo" testModulo,
                    TestLabel "testAlternative" testAlternative,
                    TestLabel "testEmptyAlternative" testEmptyAlternative,
                    TestLabel "testParseChar" testParseChar,
                    TestLabel "testParseCharFail" testParseCharFail,
                    TestLabel "testParseAnyChar" testParseAnyChar,
                    TestLabel "testParseAnyCharFail" testParseAnyCharFail,
                    TestLabel "testParseOr" testParseOr,
                    TestLabel "testParseOr2" testParseOr2,
                    TestLabel "testParseAnd" testParseAnd,
                    TestLabel "testParseAndFail" testParseAndFail,
                    TestLabel "testParseAndFail2" testParseAndFail2,
                    TestLabel "testParseAndWith" testParseAndWith,
                    TestLabel "testParseAndWithFail" testParseAndWithFail,
                    TestLabel "testParseAndWithFail2" testParseAndWithFail2,
                    TestLabel "testParseMany" testParseMany,
                    TestLabel "testParseManyFail" testParseManyFail,
                    TestLabel "testParseManyFail2" testParseManyFail2,
                    TestLabel "testParseSome" testParseSome,
                    TestLabel "testParseSomeFail" testParseSomeFail,
                    TestLabel "testParseSomeFail2" testParseSomeFail2,
                    TestLabel "testParseNoneOf" testParseNoneOf,
                    TestLabel "testParseNoneOfFail" testParseNoneOfFail,
                    TestLabel "testParseUInt" testParseUInt,
                    TestLabel "testParseUIntFail" testParseUIntFail,
                    TestLabel "testParseInt" testParseInt,
                    TestLabel "testParseInt2" testParseInt2,
                    TestLabel "testParseIntFail" testParseIntFail,
                    TestLabel "testParseQuotedSymbol" testParseQuotedSymbol,
                    TestLabel "testParseQuotedSymbolFail" testParseQuotedSymbolFail,
                    TestLabel "testParseSymbol" testParseSymbol,
                    TestLabel "testParseSymbolFail" testParseSymbolFail,
                    TestLabel "testParseWhiteSpace" testParseWhiteSpace,
                    TestLabel "testParseWhiteSpaceFail" testParseWhiteSpaceFail
                    -- TestLabel "testParseString" testParseString
                    -- TestLabel "testParseStringFail" testParseStringFail
                ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess
