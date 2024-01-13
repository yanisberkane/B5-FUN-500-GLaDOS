import Test.HUnit
-- import Interpreter (evaluate)
import Types (Ast(..), Env)
import qualified Data.Map as Map
import ErrorHandler (ErrorType(..))
import Parser
import Control.Applicative
import System.Exit
import VMExec (execute)
import VMTypes

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

testMultipleConditionalOperations :: Test
testMultipleConditionalOperations = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 5), Push (IntValue 5), Push (Operator Eq), CallOp, Push (IntValue 4), Push (IntValue 3), Push (Operator Eq), CallOp, Push (IntValue 6), Push (IntValue 6), Push (Operator Eq), CallOp, Push (Operator And), CallOp, Push (Operator And), CallOp, JumpIfFalse 3,  Push (BoolValue True), PushToOutput, Jump 2, Push (BoolValue False), PushToOutput, Push (StringValue "outside"), PushToOutput], [], [], [])
    let expected = Right ([],[],[],[],[BoolValue False,StringValue "outside"])
    assertEqual "execute [] [] ([], [Push (IntValue 3), Push (IntValue 2), Push (Operator Sup), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

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
                    TestLabel "testMultipleConditionalOperations" testMultipleConditionalOperations
                ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess
