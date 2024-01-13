import Test.HUnit ( assertEqual, runTestTT, Counts(errors, failures), Test(..) )
-- import Interpreter (evaluate)
import Types (Ast(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))
import Parser
import Control.Applicative
import System.Exit
import VMExec (execute)
import VMTypes

import CCSAstParserInt
import CCSAstParserBool
import CCSAstParserSymbol
import CCSAstParserString
import CCSAstParserSeparator
import CCSAstParserArgList
import CCSAstParserBody
import CCSAstParserLogicOperators
import CCSAstParserMathOperators
import CCSAstParserDefine
import CCSAstParserAssign
import CCSAstParserLambda
import CCSAstParserList
import CCSAstParserCall
import CCSAstParserIf
import CCSAstParserNamedCall
import BufferToCCSAstParser
import ParserTests
import ParserTests (testParseAndFail2)
-- Tests for the VM

--- Example testPushVM
-- Push 42
-- Ret
-- # execute => 42

testPushVM :: Test
testPushVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 42), Ret], [], [], [])
    let expected = Right ([IntValue 42],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 42), Ret])" expected (executed)

--- Example testAddVM
-- Push 42
-- Push 1
-- Push Add
-- Call
-- Ret
-- # execute => 43

testAddVM :: Test
testAddVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 42), Push (IntValue 1), Push (Operator Add), CallOp, Ret], [], [], [])
    let expected = Right ([IntValue 43],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 42), Push (IntValue 1), Push (Operator Add), CallOp, Ret])" expected (executed)

--- Example testPushFailVM
-- Push 10
-- Push Add
-- Call
-- Ret
-- # execute => Error: Add needs two arguments

testPushFailVM :: Test
testPushFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Add), CallOp, Ret], [], [], [])
    let expected = Left "Error: Add needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Add), CallOp, Ret])" expected (executed)

--- Example testDivFailVM
-- Push 0
-- Push 42
-- Push Div
-- Call
-- Ret
-- # exec => Error : division by 0

testDivFailVM :: Test
testDivFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 0), Push (IntValue 42), Push (Operator Div), CallOp, Ret], [], [], [])
    let expected = Left "Error: division by 0"
    assertEqual "execute [] [] ([], [Push (IntValue 0), Push (IntValue 42), Push (Operator Div), CallOp, Ret])" expected (executed)

--- Example testEqVM
-- Push 10
-- Push 10
-- Push Eq
-- Call
-- Ret
-- # exec => True

testEqVM :: Test
testEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])" expected (executed)

--- Example testSupVM
-- Push 2
-- Push 5
-- Push Sup
-- Call
-- # exec => False

testSupVM :: Test
testSupVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 2), Push (IntValue 5), Push (Operator Sup), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 2), Push (IntValue 5), Push (Operator Sup), CallOp, Ret])" expected (executed)

--- Example testJumpIfFalse1VM
-- Push 10
-- Push 10
-- Push Eq
-- Call
-- JumpIfFalse 2
-- Push 1
-- Ret
-- Push 2
-- Ret
-- # exec => 1

testJumpIfFalse1VM :: Test
testJumpIfFalse1VM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret], [], [], [])
    let expected = Right ([IntValue 1],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

-- Example testJumpIfFalse2VM
-- PushArg 0
-- Push 0
-- Push Less
-- Call
-- JumpIfFalse 2
-- PushArg 0
-- Ret
-- PushArg 0
-- Push -1
-- Push Mul
-- Call
-- Ret
-- # exec 42 => 42
-- # exec -42 => 42

testJumpIfFalse2VM :: Test
testJumpIfFalse2VM = TestCase $ do
    let executed1 = execute [(IntValue 42)] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Sup), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret], [], [], [])
    let expected1 = Right ([IntValue 42], [], [], [], [])
    assertEqual "execute [(IntValue 42)] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Sup), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret])" expected1 (executed1)
    let executed2 = execute [(IntValue (-42))] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Sup), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret], [], [], [])
    let expected2 = Right ([IntValue 42], [], [], [], [])
    assertEqual "execute [(IntValue (-42))] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Sup), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret])" expected2 (executed2)

--- Example testEnvVM
-- env =
--     absCode =
--         PushArg 0
--         Push 0
--         Push Less
--         Call
--         JumpIfFalse 2
--         PushArg 0
--         Ret
--         PushArg 0
--         Push -1
--         Push Mul
--         Call
--         Ret
--         Push -42
--         Push absCode
--         Call
--         Ret
--         exec => 42

-- Push -42
-- PushVMEnv "absCode"
-- Call
-- Ret
-- # exec => 42

testEnvVM :: Test
testEnvVM = TestCase $ do
    let executed = execute [] [("absCode", (Function [PushArg 0, Push (IntValue 0), Push (Operator Sup), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue (-42)), PushVMEnv "absCode", Call 1, Ret], [], [], [])
    let expected = Right ([IntValue 42], [], [], [], [])
    assertEqual "execute [] [(\"absCode\", (Function [PushArg 0, Push (IntValue 0), Push (Operator Sup), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue (-42)), PushVMEnv \"absCode\", Call 1, Ret])" expected (executed)

--- Example testFactorialVM
-- env =
--     fact =
--         PushArg 0
--         Push 1
--         Push Eq
--         Call
--         JumpIfFalse 2
--         Push 1
--         Ret
--         Push 1
--         PushArg 0
--         Push Sub
--         Call
--         PushVMEnv "fact"
--         Call 1
--         PushArg 0
--         Push Mul
--         Call
--         Ret

-- Push 5
-- PushVMEnv "fact"
-- Call 1
-- Ret
-- # exec => 120

testFactorialVM :: Test
testFactorialVM = TestCase $ do
    let executed = execute [] [("fact", (Function [PushArg 0, Push (IntValue 1), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 1), PushArg 0, Push (Operator Sub), CallOp, PushVMEnv "fact", Call 1, PushArg 0, Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue 5), PushVMEnv "fact", Call 1, Ret], [], [], [])
    let expected = Right ([IntValue 120], [], [], [], [])
    assertEqual "execute [] [(\"fact\", (Function [PushArg 0, Push (IntValue 1), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 1), PushArg 0, Push (Operator Sub), CallOp, PushVMEnv \"fact\", Call 1, PushArg 0, Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue 5), PushVMEnv \"fact\", Call 1, Ret])" expected (executed)

--- Example testConcatVM
-- Stack =
--     "Hello, "
--     "World!"

-- Push (Operator Concat)
-- Call
-- Ret
-- # exec => "Hello, World!"

testConcatVM :: Test
testConcatVM = TestCase $ do
    let executed = execute [] [] ([StringValue "Hello, ", StringValue "World!"], [Push (Operator Concat), CallOp, Ret], [], [], [])
    let expected = Right ([StringValue "Hello, World!"], [], [], [], [])
    assertEqual "execute [] [] ([StringValue \"Hello, \", StringValue \"World!\"], [Push (Operator Concat), CallOp, Ret])" expected (executed)

--- Example testFunctionVM
-- env =
--     test =
--         PushArg 0
--         PushArg 1
--         Push Add
--         Call
--         Ret

-- Push 24
-- Push 12
-- PushVMEnv "test"
-- Call 2
-- Ret
-- # exec => 36

testFunctionVM :: Test
testFunctionVM = TestCase $ do
    let executed = execute [] [("test", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, Ret]))] ([], [Push (IntValue 24), Push (IntValue 12), PushVMEnv "test", Call 2, Ret], [], [], [])
    let expected = Right ([IntValue 36], [], [], [], [])
    assertEqual "execute [] [(\"test\", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, Ret]))] ([], [Push (IntValue 24), Push (IntValue 12), PushVMEnv \"test\", Call 2, Ret])" expected (executed)

-- Example testPushToOutputVM
-- env =
--     fact =
--         PushArg 0
--         Push 1
--         Push Eq
--         CallOp
--         JumpIfFalse 2
--         Push 1
--         PushToOutput
--         Ret
--         Push 1
--         PushArg 0
--         Push Sub
--         CallOp
--         PushVMEnv "fact"
--         Call 1
--         PushArg 0
--         Push Mul
--         CallOp
--         Ret

-- Push 3
-- PushVMEnv "fact"
-- Call 1
-- PushToOutput
-- Push (StringValue "Hello World!")
-- PushToOutput
-- Ret
-- # exec => output: 6\nHello World!\n

testPushToOutputVM :: Test
testPushToOutputVM = TestCase $ do
    let executed = execute [] [("fact", (Function [PushArg 0, Push (IntValue 1), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 1), PushArg 0, Push (Operator Sub), CallOp, PushVMEnv "fact", Call 1, PushArg 0, Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue 3), PushVMEnv "fact", Call 1, PushToOutput, Push (StringValue "Hello World!"), PushToOutput, Ret], [], [], [])
    let expected = Right ([], [], [], [], [IntValue 6, StringValue "Hello World!"])
    assertEqual "execute [] [(\"fact\", (Function [PushArg 0, Push (IntValue 1), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), PushToOutput, Ret, Push (IntValue 1), PushArg 0, Push (Operator Sub), CallOp, PushVMEnv \"fact\", Call 1, PushArg 0, Push (Operator Mul), CallOp, PushToOutput, Ret]))] ([], [Push (IntValue 3), PushVMEnv \"fact\", Call 1, Push (StringValue \"Hello World!\"), PushToOutput, Ret])" expected (executed)

-- Example testMultipleConditionalOperationsVM
-- Push 5
-- Push 5
-- Push Eq
-- CallOp
-- Push 4
-- Push 3
-- Push Eq
-- CallOp
-- Push 6
-- Push 6
-- Push Eq
-- CallOp
-- Push And
-- CallOp
-- Push And
-- CallOp
-- JumpIfFalse 3
-- Push True
-- PushToOutput
-- Jump 2
-- Push False
-- PushToOutput
-- Push (StringValue "outside")
-- PushToOutput
-- Ret
-- # exec => output: False\noutside\n

testMultipleConditionalOperationsVM :: Test
testMultipleConditionalOperationsVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 5), Push (IntValue 5), Push (Operator Eq), CallOp, Push (IntValue 4), Push (IntValue 3), Push (Operator Eq), CallOp, Push (IntValue 6), Push (IntValue 6), Push (Operator Eq), CallOp, Push (Operator And), CallOp, Push (Operator And), CallOp, JumpIfFalse 3,  Push (BoolValue True), PushToOutput, Jump 2, Push (BoolValue False), PushToOutput, Push (StringValue "outside"), PushToOutput], [], [], [])
    let expected = Right ([],[],[],[],[BoolValue False,StringValue "outside"])
    assertEqual "execute [] [] ([], [Push (IntValue 3), Push (IntValue 2), Push (Operator Sup), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

-- Example testArithmeticOperationsVM
-- Push 5
-- Push 10
-- Push 12
-- Push Sub
-- CallOp
-- Push Add
-- CallOp
-- AssignEnvValue "x"
-- PushVMEnv "x"
-- Ret
-- # exec => 7

testArithmeticOperationsVM :: Test
testArithmeticOperationsVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 5), Push (IntValue 10), Push (IntValue 12), Push (Operator Sub), CallOp, Push (Operator Add), CallOp, AssignEnvValue "x", PushVMEnv "x", Ret], [], [], [])
    let expected = Right ([IntValue 7],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 5), Push (IntValue 10), Push (IntValue 12), Push (Operator Sub), CallOp, Push (Operator Add), CallOp, AssignEnvValue \"x\", Ret])" expected (executed)

-- Example testArithmeticOperationsVM2
-- Push 1
-- Push 1
-- Push Add
-- CallOp
-- AssignEnvValue "b"
-- Push 2
-- Push 2
-- Push Add
-- CallOp
-- AssignEnvValue "a"
-- PushVMEnv "b"
-- PushVMEnv "a"
-- Push Add
-- CallOp
-- AssignEnvValue "a"
-- PushVMEnv "a"
-- Ret
-- # exec => 6

testArithmeticOperationsVM2 :: Test
testArithmeticOperationsVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 1), Push (IntValue 1), Push (Operator Add), CallOp, AssignEnvValue "b", Push (IntValue 2), Push (IntValue 2), Push (Operator Add), CallOp, AssignEnvValue "a", PushVMEnv "b", PushVMEnv "a", Push (Operator Add), CallOp, AssignEnvValue "a", PushVMEnv "a", Ret], [], [], [])
    let expected = Right ([IntValue 6],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 1), Push (IntValue 1), Push (Operator Add), CallOp, AssignEnvValue \"b\", Push (IntValue 2), Push (IntValue 2), Push (Operator Add), CallOp, AssignEnvValue \"a\", PushVMEnv \"b\", PushVMEnv \"a\", Push (Operator Add), CallOp, AssignEnvValue \"a\", PushVMEnv \"a\", Ret])" expected (executed)

-- Example testArithmeticOperationsVM3
-- Push 1
-- Push 1
-- Push Add
-- CallOp
-- AssignEnvValue "b"
-- Push 2
-- Push 2
-- Push Add
-- CallOp
-- AssignEnvValue "a"
-- PushVMEnv "b"
-- PushVMEnv "a"
-- Push Add
-- CallOp
-- AssignEnvValue "a"
-- PushVMEnv "a"
-- Ret
-- # exec => 6

testAssignmentVM :: Test
testAssignmentVM = TestCase $ do
    let executed = execute [] [("a", (IntValue 1)), ("b", (IntValue 1))] ([], [PushVMEnv "a", PushVMEnv "b", Push (Operator Eq), CallOp, JumpIfFalse 3, Push (IntValue 10), AssignEnvValue "x", Jump 2, Push (IntValue 11), AssignEnvValue "x", PushVMEnv "x", Ret], [], [], [])
    let expected = Right ([IntValue 10],[],[],[],[])
    assertEqual "execute [] [(\"a\", (IntValue 1)), (\"b\", (IntValue 1))] ([], [PushVMEnv \"a\", PushVMEnv \"b\", Push (Operator Eq), CallOp, JumpIfFalse 3, Push (IntValue 10), AssignEnvValue \"x\", Jump 2, Push (IntValue 11), AssignEnvValue \"x\", PushVMEnv \"x\", Ret])" expected (executed)

-- Example testAssignmentVM2
-- Push "hi"
-- PushVMEnv "test"
-- Call 1
-- Ret
-- # exec => "hi"
-- env =
--     test =
--         PushArg 0
--         AssignEnvValue "x"
--         PushVMEnv "x"
--         Ret

testAssignmentVM2 :: Test
testAssignmentVM2 = TestCase $ do
    let executed = execute [] [("test", (Function [PushArg 0, AssignEnvValue "x", PushVMEnv "x", Ret]))] ([], [Push (StringValue "hi"), PushVMEnv "test", Call 1, Ret], [], [], [])
    let expected = Right ([StringValue "hi"],[],[],[],[])
    assertEqual "execute [] [(\"test\", (Function [PushArg 0, AssignEnvValue \"x\", PushVMEnv \"x\", Ret]))] ([], [Push (StringValue \"hi\"), PushVMEnv \"test\", Call 1, Ret])" expected (executed)

-- Example testAssignmentVM3
-- PushVMEnv "a"
-- PushVMEnv "b"
-- Push (Operator Add)
-- CallOp
-- PushVMEnv "c"
-- Push (Operator Add)
-- CallOp
-- AssignEnvValue "d"

testAssignmentVM3 :: Test
testAssignmentVM3 = TestCase $ do
    let executed = execute [] [("a", (IntValue 5)), ("b", (IntValue 10)), ("c", (IntValue 3)), ("test2", (Function [Push (IntValue 45), Ret])), ("test", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue "d", PushVMEnv "d", Ret]))] ([], [PushVMEnv "a", PushVMEnv "b", PushVMEnv "c", PushVMEnv "test", Call 3, Ret], [], [], [])
    let expected = Right ([IntValue 18],[],[],[],[])
    assertEqual "execute [] [(\"a\", (IntValue 5)), (\"b\", (IntValue 10)), (\"c\", (IntValue 3)), (\"test2\", (Function [Push (IntValue 45), Ret])), (\"test\", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue \"d\", PushVMEnv \"d\", Ret]))] ([], [PushVMEnv \"a\", PushVMEnv \"b\", PushVMEnv \"c\", PushVMEnv \"test\", Call 3, Ret])" expected (executed)

-- Example testArithmeticOperationsVM6
-- PushVMEnv "test"
-- Call 0
-- Ret
-- # exec => 5
-- env =
--     test =
--         Push 5
--         AssignEnvValue "x"
--         PushVMEnv "x"
--         Ret

testAssignmentVM4 :: Test
testAssignmentVM4 = TestCase $ do
    let executed = execute [] [("test", (Function [Push (IntValue 5), AssignEnvValue "x", PushVMEnv "x"]))] ([], [PushVMEnv "test", Call 0, Ret], [], [], [])
    let expected = Right ([IntValue 5],[],[],[],[])
    assertEqual "execute [] [(\"test\", (Function [Push (IntValue 5), AssignEnvValue \"x\", PushVMEnv \"x\", Ret]))] ([], [PushVMEnv \"test\", Call 0, Ret])" expected (executed)

tests :: Test
tests = TestList [
                    TestLabel "testPushVM" testPushVM,
                    TestLabel "testAddVM" testAddVM,
                    TestLabel "testPushFailVM" testPushFailVM,
                    TestLabel "testDivFailVM" testDivFailVM,
                    TestLabel "testEqVM" testEqVM,
                    TestLabel "testSupVM" testSupVM,
                    TestLabel "testJumpIfFalse1VM" testJumpIfFalse1VM,
                    TestLabel "testJumpIfFalse2VM" testJumpIfFalse2VM,
                    TestLabel "testEnvVM" testEnvVM,
                    TestLabel "testFactorialVM" testFactorialVM,
                    TestLabel "testConcatVM" testConcatVM,
                    TestLabel "testFunctionVM" testFunctionVM,
                    TestLabel "testPushToOutputVM" testPushToOutputVM,
                    TestLabel "testMultipleConditionalOperationsVM" testMultipleConditionalOperationsVM,
                    TestLabel "testArithmeticOperationsVM" testArithmeticOperationsVM,
                    TestLabel "testArithmeticOperationsVM2" testArithmeticOperationsVM2,
                    TestLabel "testAssignmentVM" testAssignmentVM,
                    TestLabel "testAssignmentVM2" testAssignmentVM2,
                    TestLabel "testAssignmentVM3" testAssignmentVM3,
                    TestLabel "testAssignmentVM4" testAssignmentVM4,
                    -- CCS to AST tests
                    -- Int
                    TestLabel "testParseAstInt" testParseAstInt,
                    TestLabel "testParseAstIntWithSpaces" testParseAstIntWithSpaces,
                    TestLabel "testParseAstIntNegative" testParseAstIntNegative,
                    TestLabel "testParseAstIntFail" testParseAstIntFail,
                    TestLabel "testParseAstIntFail2" testParseAstIntFail2,
                    -- Bool
                    TestLabel "testParseAstBoolTrue" testParseAstBoolTrue,
                    TestLabel "testParseAstBoolFalse" testParseAstBoolFalse,
                    TestLabel "testParseAstBoolFail" testParseAstBoolFail,
                    TestLabel "testParseAstBoolFail2" testParseAstBoolFail2,
                    -- String
                    TestLabel "testParseAstString" testParseAstString,
                    TestLabel "testParseAstString2" testParseAstString2,
                    TestLabel "testParseAstStringWithSpaces" testParseAstStringWithSpaces,
                    TestLabel "testParseAstStringWithNumber" testParseAstStringWithNumber,
                    TestLabel "testParseAstStringFail" testParseAstStringFail,
                    -- Symbol
                    TestLabel "testParseAstSymbol" testParseAstSymbol,
                    TestLabel "testParseAstSymbolWithSpaces" testParseAstSymbolWithSpaces,
                    TestLabel "testParseAstSymbolWithNumber" testParseAstSymbolWithNumber,
                    TestLabel "testParseAstSymbolWithNumberAndUnderscore" testParseAstSymbolWithNumberAndUnderscore,
                    TestLabel "testParseAstSymbolFail" testParseAstSymbolFail,
                    TestLabel "testParseAstSymbolFail2" testParseAstSymbolFail2,
                    -- Separator
                    TestLabel "testParseAstSeparator" testParseAstSeparator,
                    TestLabel "testParseAstSeparatorWithSpaces" testParseAstSeparatorWithSpaces,
                    TestLabel "testParseAstSeparatorFail" testParseAstSeparatorFail,
                    -- ArgList
                    TestLabel "testParseAstArgList" testParseAstArgList,
                    TestLabel "testParseAstArgListWithSpaces" testParseAstArgListWithSpaces,
                    TestLabel "testParseAstArgListFail" testParseAstArgListFail,
                    -- Body
                    TestLabel "testParseAstBody" testParseAstBody,
                    TestLabel "testParseAstBodyWithSpaces" testParseAstBodyWithSpaces,
                    TestLabel "testParseAstBodyFail" testParseAstBodyFail,
                    -- LogicOperators
                    TestLabel "testParseAstLogicOperatorAnd" testParseAstLogicOperatorAnd,
                    TestLabel "testParseAstLogicOperatorOr" testParseAstLogicOperatorOr,
                    TestLabel "testParseAstLogicOperatorEqual" testParseAstLogicOperatorEqual,
                    TestLabel "testParseAstLogicOperatorNotEqual" testParseAstLogicOperatorNotEqual,
                    TestLabel "testParseAstLogicOperatorLessThan" testParseAstLogicOperatorLessThan,
                    TestLabel "testParseAstLogicOperatorLessThanOrEqual" testParseAstLogicOperatorLessThanOrEqual,
                    TestLabel "testParseAstLogicOperatorGreaterThan" testParseAstLogicOperatorGreaterThan,
                    TestLabel "testParseAstLogicOperatorGreaterThanOrEqual" testParseAstLogicOperatorGreaterThanOrEqual,
                    TestLabel "testParseAstLogicOperatorNot" testParseAstLogicOperatorNot,
                    -- MathOperators
                    TestLabel "testParseAstMathOperatorAdd" testParseAstOperatorAdd,
                    TestLabel "testParseAstOperatorSub" testParseAstOperatorSub,
                    TestLabel "testParseAstOperatorMul" testParseAstOperatorMul,
                    TestLabel "testParseAstOperatorDiv" testParseAstOperatorDiv,
                    TestLabel "testParseAstOperatorMod" testParseAstOperatorMod,
                    -- Define
                    TestLabel "testParseAstDefineLet" testParseAstDefineLet,
                    TestLabel "testParseAstDefineVar" testParseAstDefineVar,
                    TestLabel "testParseAstDefineLetWithSpaces" testParseAstDefineLetWithSpaces,
                    TestLabel "testParseAstDefineVarWithSpaces" testParseAstDefineVarWithSpaces,
                    TestLabel "testParseAstDefineFail" testParseAstDefineFail,
                    TestLabel "testParseAstDefineFail2" testParseAstDefineFail2,
                    TestLabel "testParseAstDefineFail3" testParseAstDefineFail3,
                    TestLabel "testParseAstDefineFail4" testParseAstDefineFail4,
                    -- Assign
                    TestLabel "testParseAstAssignInt" testParseAstAssignInt,
                    TestLabel "testParseAstAssignString" testParseAstAssignString,
                    TestLabel "testParseAstAssignBool" testParseAstAssignBool,
                    TestLabel "testParseAstAssignBool2" testParseAstAssignBool2,
                    TestLabel "testParseAstAssignWithSpaces" testParseAstAssignWithSpaces,
                    TestLabel "testParseAstAssignFail" testParseAstAssignFail,
                    -- Lambda
                    TestLabel "testParseAstLambda" testParseAstLambda,
                    TestLabel "testParseAstLambda2" testParseAstLambda2,
                    TestLabel "testParseAstLambda3" testParseAstLambda3,
                    -- List
                    TestLabel "testParseAstList" testParseAstList,
                    TestLabel "testParseAstListFail" testParseAstListFail,
                    -- Call
                    TestLabel "testParseCall" testParseAstCall,
                    TestLabel "testParseCall2" testParseAstCall2,
                    TestLabel "testParseCallFail" testParseAstCallFail,
                    -- If
                    TestLabel "testParseIf" testParseAstIf,
                    TestLabel "testParseIfElse" testParseAstIfElse,
                    TestLabel "testParseIfTernary" testParseAstIfTernary,
                    TestLabel "testParseIfFail" testParseAstIfFail,
                    -- NamedCall
                    TestLabel "testParseNamedCall" testParseAstNamedCall,
                    TestLabel "testParseNamedCall2" testParseAstNamedCall2,
                    TestLabel "testParseNamedCall3" testParseAstNamedCall3,
                    -- BufferToCCSAstParser
                    TestLabel "testParseBufferToCCSAst" testParseBufferToCCSAst,
                    TestLabel "testParseBufferToCCSAst2" testParseBufferToCCSAst2,
                    TestLabel "testParseBufferToCCSAstFail" testParseBufferToCCSAstFail,
                    -- Parser
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
                    TestLabel "testParseManyFail3" testParseManyFail3,
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
                ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess
