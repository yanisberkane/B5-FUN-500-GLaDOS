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

testPushVM :: Test
testPushVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 42), Ret], [], [], [])
    let expected = Right ([IntValue 42],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 42), Ret])" expected (executed)

testAddVM :: Test
testAddVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 42), Push (IntValue 1), Push (Operator Add), CallOp, Ret], [], [], [])
    let expected = Right ([IntValue 43],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 42), Push (IntValue 1), Push (Operator Add), CallOp, Ret])" expected (executed)

testPushFailVM :: Test
testPushFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Add), CallOp, Ret], [], [], [])
    let expected = Left "Error: Add needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Add), CallOp, Ret])" expected (executed)

testDivFailVM :: Test
testDivFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 0), Push (IntValue 42), Push (Operator Div), CallOp, Ret], [], [], [])
    let expected = Left "Error: division by 0"
    assertEqual "execute [] [] ([], [Push (IntValue 0), Push (IntValue 42), Push (Operator Div), CallOp, Ret])" expected (executed)

testEqVM :: Test
testEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])" expected (executed)

testLessVM :: Test
testLessVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 2), Push (IntValue 5), Push (Operator Less), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 2), Push (IntValue 5), Push (Operator Less), CallOp, Ret])" expected (executed)

testJumpIfFalse1VM :: Test
testJumpIfFalse1VM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret], [], [], [])
    let expected = Right ([IntValue 1],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

testJumpIfFalse2Vm :: Test
testJumpIfFalse2Vm = TestCase $ do
    let executed1 = execute [(IntValue 42)] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret], [], [], [])
    let expected1 = Right ([IntValue 42], [], [], [], [])
    assertEqual "execute [(IntValue 42)] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret])" expected1 (executed1)
    let executed2 = execute [(IntValue (-42))] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret], [], [], [])
    let expected2 = Right ([IntValue 42], [], [], [], [])
    assertEqual "execute [(IntValue (-42))] [] ([], [PushArg 0, Push (IntValue 0), Push (Operator Less), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret])" expected2 (executed2)

testEnvVM :: Test
testEnvVM = TestCase $ do
    let executed = execute [] [("absCode", (Function [PushArg 0, Push (IntValue 0), Push (Operator Less), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue (-42)), PushVMEnv "absCode", Call 1, Ret], [], [], [])
    let expected = Right ([IntValue 42], [], [], [], [])
    assertEqual "execute [] [(\"absCode\", (Function [PushArg 0, Push (IntValue 0), Push (Operator Less), CallOp, JumpIfFalse 2, PushArg 0, Ret, PushArg 0, Push (IntValue (-1)), Push (Operator Mul), CallOp, Ret]))] ([], [Push (IntValue (-42)), PushVMEnv \"absCode\", Call 1, Ret])" expected (executed)

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
                    TestLabel "testEnvVM" testEnvVM
                ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess
