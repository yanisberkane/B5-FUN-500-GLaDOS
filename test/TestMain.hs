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
import ErrorHandlerTests
import FormatterTests
import InterpreterTests

-- data Ast = AstInt Int
--     7          | AstBool Bool
--     8          | AstSymbol String
--     9          | AstString String
--    10          | AstList [Ast]
--    11          | AstMathOp Ast Ast Ast
--    12          | Assign Ast Ast
--    13          | Define Ast Ast
--    14          | If Ast Ast Ast
--    15          | Lambda Ast Ast
--    16          | NamedCall Ast Ast
--    17          | AstCall Ast Ast
--    18          | Separator Char
--    19          | AstOperator String
--    20          | LogicOperator String
--    21          | AstNone
--    22          deriving (Show, Eq)


-- test show for Ast

testShowAstInt :: Test
testShowAstInt = TestCase $ do
    let ast = AstInt 42
    let expected = "AstInt 42"
    assertEqual "show AstInt 42" expected (show ast)

testShowAstString :: Test
testShowAstString = TestCase $ do
    let ast = AstString "Hello World!"
    let expected = "AstString \"Hello World!\""
    assertEqual "show AstString \"Hello World!\"" expected (show ast)

testShowAstBool :: Test
testShowAstBool = TestCase $ do
    let ast = AstBool True
    let expected = "AstBool True"
    assertEqual "show AstBool True" expected (show ast)

testShowAstNone :: Test
testShowAstNone = TestCase $ do
    let ast = AstNone
    let expected = "AstNone"
    assertEqual "show AstNone" expected (show ast)

testShowAstSymbol :: Test
testShowAstSymbol = TestCase $ do
    let ast = AstSymbol "x"
    let expected = "AstSymbol \"x\""
    assertEqual "show AstSymbol \"x\"" expected (show ast)

testShowAstList :: Test
testShowAstList = TestCase $ do
    let ast = AstList [AstInt 42, AstString "Hello World!", AstBool True]
    let expected = "AstList [AstInt 42,AstString \"Hello World!\",AstBool True]"
    assertEqual "show AstList [AstInt 42, AstString \"Hello World!\", AstBool True]" expected (show ast)

testShowAstMathOp :: Test
testShowAstMathOp = TestCase $ do
    let ast = AstMathOp (AstInt 42) (AstInt 42) (AstOperator "+")
    let expected = "AstMathOp (AstInt 42) (AstInt 42) (AstOperator \"+\")"
    assertEqual "show AstMathOp (AstInt 42) (AstInt 42) (AstOperator \"+\")" expected (show ast)


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

testCallOpFailVM :: Test
testCallOpFailVM = TestCase $ do
    let executed = execute [] [] ([], [CallOp, Ret], [], [], [])
    let expected = Left "Error: CallOp needs an operator on top of the stack"
    assertEqual "execute [] [] ([], [CallOp, Ret])" expected (executed)

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

testJumpIfTrue1VM :: Test
testJumpIfTrue1VM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, JumpIfTrue 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret], [], [], [])
    let expected = Right ([IntValue 2],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, JumpIfTrue 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

testJumpIfTrue2VM :: Test
testJumpIfTrue2VM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 11), Push (Operator Eq), CallOp, JumpIfTrue 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret], [], [], [])
    let expected = Right ([IntValue 1],[], [], [], [])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 11), Push (Operator Eq), CallOp, JumpIfTrue 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

testJumpIfFalseFailVM :: Test
testJumpIfFalseFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret], [], [], [])
    let expected = Left "Error: JumpIfFalse needs a boolean on top of the stack"
    assertEqual "execute [] [] ([], [Push (IntValue 10), JumpIfFalse 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

testJumpIfTrueFailVM :: Test
testJumpIfTrueFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), JumpIfTrue 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret], [], [], [])
    let expected = Left "Error: JumpIfTrue needs a boolean on top of the stack"
    assertEqual "execute [] [] ([], [Push (IntValue 10), JumpIfTrue 2, Push (IntValue 1), Ret, Push (IntValue 2), Ret])" expected (executed)

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

testAssignEnvValueFailVM :: Test
testAssignEnvValueFailVM = TestCase $ do
    let executed = execute [] [] ([], [AssignEnvValue "x", PushVMEnv "x", Ret], [], [], [])
    let expected = Left "Error: AssignEnvValue needs a value on top of the stack"
    assertEqual "execute [] [] ([], [AssignEnvValue \"x\", Ret])" expected (executed)

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

testPushVMEnvFailVM :: Test
testPushVMEnvFailVM = TestCase $ do
    let executed = execute [] [("test2", (Function [Push (IntValue 45), Ret])), ("test", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue "d", PushVMEnv "d", Ret]))] ([], [PushVMEnv "a", PushVMEnv "b", PushVMEnv "c", PushVMEnv "test", Call 3, Ret], [], [], [])
    let expected = Left "Error: Variable a not found"
    assertEqual "execute [] [(\"a\", (IntValue 5)), (\"b\", (IntValue 10)), (\"c\", (IntValue 3)), (\"test2\", (Function [Push (IntValue 45), Ret])), (\"test\", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue \"d\", PushVMEnv \"d\", Ret]))] ([], [PushVMEnv \"a\", PushVMEnv \"b\", PushVMEnv \"c\", PushVMEnv \"test\", Call 3, Ret])" expected (executed)

testPushArgsOutOfBounds :: Test
testPushArgsOutOfBounds = TestCase $ do
    let executed = execute [] [("a", (IntValue 5)), ("b", (IntValue 10)), ("c", (IntValue 3)), ("test2", (Function [Push (IntValue 45), Ret])), ("test", (Function [PushArg 0, PushArg 7, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue "d", PushVMEnv "d", Ret]))] ([], [PushVMEnv "a", PushVMEnv "b", PushVMEnv "c", PushVMEnv "test", Call 3, Ret], [], [], [])
    let expected = Left "Error: Argument index 7 out of bounds"
    assertEqual "execute [] [(\"a\", (IntValue 5)), (\"b\", (IntValue 10)), (\"c\", (IntValue 3)), (\"test2\", (Function [Push (IntValue 45), Ret])), (\"test\", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue \"d\", PushVMEnv \"d\", Ret]))] ([], [PushVMEnv \"a\", PushVMEnv \"b\", PushVMEnv \"c\", PushVMEnv \"test\", Call 3, Ret])" expected (executed)

testAssignmentFailVM :: Test
testAssignmentFailVM = TestCase $ do
    let executed = execute [] [("a", (IntValue 5)), ("b", (IntValue 10)), ("c", (IntValue 3)), ("test2", (Function [Push (IntValue 45), Ret])), ("test", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue "d", PushVMEnv "d", Ret]))] ([], [PushVMEnv "a", PushVMEnv "b", PushVMEnv "test", Call 3, Ret], [], [], [])
    let expected = Left "Error: Call needs 3 arguments"
    assertEqual "execute [] [(\"a\", (IntValue 5)), (\"b\", (IntValue 10)), (\"c\", (IntValue 3)), (\"test2\", (Function [Push (IntValue 45), Ret])), (\"test\", (Function [PushArg 0, PushArg 1, Push (Operator Add), CallOp, PushArg 2, Push (Operator Add), CallOp, AssignEnvValue \"d\", PushVMEnv \"d\", Ret]))] ([], [PushVMEnv \"a\", PushVMEnv \"b\", PushVMEnv \"test\", Call 3, Ret])" expected (executed)

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

-- generate more tests to cover all cases of the VM
-- test the Left cases for operations

testSubFailVM :: Test
testSubFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Sub), CallOp, Ret], [], [], [])
    let expected = Left "Error: Sub needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Sub), CallOp, Ret])" expected (executed)

testMulFailVM :: Test
testMulFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Mul), CallOp, Ret], [], [], [])
    let expected = Left "Error: Mul needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Mul), CallOp, Ret])" expected (executed)

testModFailVM :: Test
testModFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Mod), CallOp, Ret], [], [], [])
    let expected = Left "Error: Mod needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Mod), CallOp, Ret])" expected (executed)

testAndFailVM :: Test
testAndFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator And), CallOp, Ret], [], [], [])
    let expected = Left "Error: And needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator And), CallOp, Ret])" expected (executed)

testOrFailVM :: Test
testOrFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Or), CallOp, Ret], [], [], [])
    let expected = Left "Error: Or needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Or), CallOp, Ret])" expected (executed)

testNotVM :: Test
testNotVM = TestCase $ do
    let executed = execute [] [] ([], [Push (Operator Not), CallOp, Ret], [], [], [])
    let expected = Left "Error: Not needs one argument"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Not), CallOp, Ret])" expected (executed)

testNotFailVM :: Test
testNotFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Not), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Not), CallOp, Ret])" expected (executed)

testEqFailVM :: Test
testEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Left "Error: Eq needs two arguments of the same type"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Eq), CallOp, Ret])" expected (executed)

testSupFailVM :: Test
testSupFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Sup), CallOp, Ret], [], [], [])
    let expected = Left "Error: Sup needs two arguments of the same type"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Sup), CallOp, Ret])" expected (executed)

testLessFailVM :: Test
testLessFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Less), CallOp, Ret], [], [], [])
    let expected = Left "Error: Less needs two arguments of the same type"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Less), CallOp, Ret])" expected (executed)

testConcatFailVM :: Test
testConcatFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Concat), CallOp, Ret], [], [], [])
    let expected = Left "Error: Concat needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Concat), CallOp, Ret])" expected (executed)

testLessEqVM :: Test
testLessEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator LessEq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator LessEq), CallOp, Ret])" expected (executed)

testLessEqFailVM :: Test
testLessEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (StringValue "10"), Push (IntValue 10), Push (Operator LessEq), CallOp, Ret], [], [], [])
    let expected = Left "Error: LessEq needs two arguments of the same type"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator LessEq), CallOp, Ret])" expected (executed)

testSupEqVM :: Test
testSupEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator SupEq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator SupEq), CallOp, Ret])" expected (executed)

testSupEqFailVM :: Test
testSupEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (StringValue "10"), Push (IntValue 10), Push (Operator SupEq), CallOp, Ret], [], [], [])
    let expected = Left "Error: SupEq needs two arguments of the same type"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator SupEq), CallOp, Ret])" expected (executed)

testNotEqVM :: Test
testNotEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator NotEq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator NotEq), CallOp, Ret])" expected (executed)

testNotEq2VM :: Test
testNotEq2VM = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue True), Push (BoolValue True), Push (Operator NotEq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (BoolValue True), Push (Operator NotEq), CallOp, Ret])" expected (executed)

testNotEqFailVM :: Test
testNotEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (StringValue "10"), Push (IntValue 10), Push (Operator NotEq), CallOp, Ret], [], [], [])
    let expected = Left "Error: NotEq needs two arguments of the same type"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator NotEq), CallOp, Ret])" expected (executed)

testOperationDivVM :: Test
testOperationDivVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 2), Push (Operator Div), CallOp, Ret], [], [], [])
    let expected = Right ([IntValue 0],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 2), Push (Operator Div), CallOp, Ret])" expected (executed)

testOperationDivFailVM :: Test
testOperationDivFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Div), CallOp, Ret], [], [], [])
    let expected = Left "Error: Div needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Div), CallOp, Ret])" expected (executed)

testOperationModVM :: Test
testOperationModVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 2), Push (Operator Mod), CallOp, Ret], [], [], [])
    let expected = Right ([IntValue 2],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 2), Push (Operator Mod), CallOp, Ret])" expected (executed)

testOperationModFailVM :: Test
testOperationModFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (Operator Mod), CallOp, Ret], [], [], [])
    let expected = Left "Error: Mod needs two arguments"
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (Operator Mod), CallOp, Ret])" expected (executed)

testOperationDivFailVM2 :: Test
testOperationDivFailVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 0), Push (Operator Div), CallOp, Ret], [], [], [])
    let expected = Right ([IntValue 0],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 0), Push (Operator Div), CallOp, Ret])" expected (executed)

testListOperationAddVM :: Test
testListOperationAddVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Add, Ret], [], [], [])
    let expected = Right ([IntValue 6],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Add, Ret])" expected (executed)

testListOperationSubVM :: Test
testListOperationSubVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Sub, Ret], [], [], [])
    let expected = Right ([IntValue (-4)],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Sub, Ret])" expected (executed)

testListOperationMulVM :: Test
testListOperationMulVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Mul, Ret], [], [], [])
    let expected = Right ([IntValue 6],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Mul, Ret])" expected (executed)

testListOperationDivVM :: Test
testListOperationDivVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Div, Ret], [], [], [])
    let expected = Right ([IntValue 0],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Div, Ret])" expected (executed)

testListOperationModVM :: Test
testListOperationModVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Mod, Ret], [], [], [])
    let expected = Right ([IntValue 1],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Mod, Ret])" expected (executed)

testListOperationAndVM :: Test
testListOperationAndVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [BoolValue True, BoolValue True, BoolValue True]), OperateOnList And, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [BoolValue True, BoolValue True, BoolValue True]), OperateOnList And, Ret])" expected (executed)

testListOperationOrVM :: Test
testListOperationOrVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [BoolValue True, BoolValue True, BoolValue True]), OperateOnList Or, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [BoolValue True, BoolValue True, BoolValue True]), OperateOnList Or, Ret])" expected (executed)

testListOperationNotVM :: Test
testListOperationNotVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [BoolValue True, BoolValue True, BoolValue True]), OperateOnList Not, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [BoolValue True, BoolValue True, BoolValue True]), OperateOnList Not, Ret])" expected (executed)

testListOperateNotVM2 :: Test
testListOperateNotVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 0, IntValue 0, IntValue 0]), OperateOnList Not, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 0, IntValue 0, IntValue 0]), OperateOnList Not, Ret])" expected (executed)

testListOperationEqVM :: Test
testListOperationEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 1, IntValue 1]), OperateOnList Eq, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 1, IntValue 1]), OperateOnList Eq, Ret])" expected (executed)

testListOperationLessVM :: Test
testListOperationLessVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Less, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList Less, Ret])" expected (executed)

testListOperationNotEqVM :: Test
testListOperationNotEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList NotEq, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 2, IntValue 3]), OperateOnList NotEq, Ret])" expected (executed)

testListOperationSupVM :: Test
testListOperationSupVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 3, IntValue 2, IntValue 1]), OperateOnList Sup, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 3, IntValue 2, IntValue 1]), OperateOnList Sup, Ret])" expected (executed)

testListOperationConcatVM :: Test
testListOperationConcatVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [StringValue "Hello", StringValue "World", StringValue "!"]), OperateOnList Concat, Ret], [], [], [])
    let expected = Right ([StringValue "HelloWorld!"],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [StringValue \"Hello\", StringValue \"World\", StringValue \"!\"]), OperateOnList Concat, Ret])" expected (executed)

testListOperationLessEqVM :: Test
testListOperationLessEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 1, IntValue 1]), OperateOnList LessEq, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 1, IntValue 1]), OperateOnList LessEq, Ret])" expected (executed)

testListOperationSupEqVM :: Test
testListOperationSupEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 1, IntValue 1]), OperateOnList SupEq, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (ListValue [IntValue 1, IntValue 1, IntValue 1]), OperateOnList SupEq, Ret])" expected (executed)

testListOperationSubFailVM :: Test
testListOperationSubFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Sub, Ret], [], [], [])
    let expected = Left "Error: Sub needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Sub, Ret])" expected (executed)

testListOperationDivFailVM :: Test
testListOperationDivFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Div, Ret], [], [], [])
    let expected = Left "Error: Div needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Div, Ret])" expected (executed)

testListOperationModFailVM :: Test
testListOperationModFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Mod, Ret], [], [], [])
    let expected = Left "Error: Mod needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Mod, Ret])" expected (executed)

testListOperationNotFailVM :: Test
testListOperationNotFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Not, Ret], [], [], [])
    let expected = Left "Error: Not needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Not, Ret])" expected (executed)

testListOperationEqFailVM :: Test
testListOperationEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Eq, Ret], [], [], [])
    let expected = Left "Error: Eq needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Eq, Ret])" expected (executed)

testListOperationLessFailVM :: Test
testListOperationLessFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Less, Ret], [], [], [])
    let expected = Left "Error: Less needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Less, Ret])" expected (executed)

testListOperationNotEqFailVM :: Test
testListOperationNotEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList NotEq, Ret], [], [], [])
    let expected = Left "Error: NotEq needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList NotEq, Ret])" expected (executed)

testListOperationSupFailVM :: Test
testListOperationSupFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList Sup, Ret], [], [], [])
    let expected = Left "Error: Sup needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList Sup, Ret])" expected (executed)

testListOperationLessEqFailVM :: Test
testListOperationLessEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList LessEq, Ret], [], [], [])
    let expected = Left "Error: LessEq needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList LessEq, Ret])" expected (executed)

testListOperationSupEqFailVM :: Test
testListOperationSupEqFailVM = TestCase $ do
    let executed = execute [] [] ([], [Push (ListValue []), OperateOnList SupEq, Ret], [], [], [])
    let expected = Left "Error: SupEq needs at least one argument"
    assertEqual "execute [] [] ([], [Push (ListValue []), OperateOnList SupEq, Ret])" expected (executed)

testOperationEqVM :: Test
testOperationEqVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])" expected (executed)

testOperationEqVM2 :: Test
testOperationEqVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue True), Push (BoolValue True), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (BoolValue True), Push (Operator Eq), CallOp, Ret])" expected (executed)

testOperatorEqVM3 :: Test
testOperatorEqVM3 = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue False), Push (IntValue 1), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue False), Push (IntValue 1), Push (Operator Eq), CallOp, Ret])" expected (executed)

testOperationEqVM3 :: Test
testOperationEqVM3 = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue True), Push (IntValue 1), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (IntValue 1), Push (Operator Eq), CallOp, Ret])" expected (executed)

testOperationEqVM4 :: Test
testOperationEqVM4 = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 1), Push (BoolValue True), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (IntValue 0), Push (Operator Eq), CallOp, Ret])" expected (executed)

testOperationEqVM5 :: Test
testOperationEqVM5 = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 0), Push (BoolValue False), Push (Operator Eq), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (IntValue 0), Push (Operator Eq), CallOp, Ret])" expected (executed)

testOperationLessVM :: Test
testOperationLessVM = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 1), Push (IntValue 2), Push (Operator Less), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 1), Push (IntValue 2), Push (Operator Less), CallOp, Ret])" expected (executed)

testOperationNotVM :: Test
testOperationNotVM = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue True), Push (Operator Not), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (Operator Not), CallOp, Ret])" expected (executed)

testOperationNotVM2 :: Test
testOperationNotVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 0), Push (Operator Not), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (IntValue 0), Push (Operator Not), CallOp, Ret])" expected (executed)

testOperationOrVM :: Test
testOperationOrVM = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue True), Push (BoolValue False), Push (Operator Or), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue True],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue True), Push (BoolValue False), Push (Operator Or), CallOp, Ret])" expected (executed)

testOperationOrVM2 :: Test
testOperationOrVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (BoolValue False), Push (BoolValue False), Push (Operator Or), CallOp, Ret], [], [], [])
    let expected = Right ([BoolValue False],[],[],[],[])
    assertEqual "execute [] [] ([], [Push (BoolValue False), Push (BoolValue False), Push (Operator Or), CallOp, Ret])" expected (executed)

testOperationModFailVM2 :: Test
testOperationModFailVM2 = TestCase $ do
    let executed = execute [] [] ([], [Push (IntValue 0), Push (IntValue 10), Push (Operator Mod), CallOp, Ret], [], [], [])
    let expected = Left "Error: division by 0"
    assertEqual "execute [] [] ([], [Push (IntValue 0), Push (IntValue 10), Push (Operator Mod), CallOp, Ret])" expected (executed)


-- data Value = IntValue Int
--     4             | BoolValue Bool
--     5             | Operator Operator
--     6             | Function Insts
--     7             -- | SymbolValue String
--     8             -- | CharValue Char
--     9             | StringValue String
--    10             | ListValue [Value]
--    11             deriving (Show, Eq)
--    12 
--    13 data Operator = Add
--    14             | Sub
--    15             | Mul
--    16             | Div
--    17             | Mod
--    18             | And
--    19             | Or
--    20             | Not
--    21             | Eq
--    22             | NotEq
--    23             | Less
--    24             | Sup
--    25             | Concat
--    26             | LessEq
--    27             | SupEq
--    28             deriving (Show, Eq)
--    29 
--    30 data Instruction = Push Value
--    31                  | Call Int
--    32                  | CallOp
--    33                  | Ret
--    34                  | JumpIfFalse Int -- Jump to instruction at index if top of stack is false
--    35                  | JumpIfTrue Int -- Jump to instruction at index if top of stack is true
--    36                  | PushArg Int -- Push argument at index of arguments list to stack
--    37                  | PushVMEnv String -- Push value of variable with name to stack
--    38                  | OperateOnList Operator -- Apply instructions to list on top of stack
--    39                  | AssignEnvValue String -- Assign value on top of stack to Env variable with name
--    40                  | PushToOutput -- Push last value of stack to output
--    41                  | Jump Int -- Jump to instruction at index
--    42                  deriving (Show, Eq)

--- test cases for VMTypes for Show and Eq

testIntValueShow :: Test
testIntValueShow = TestCase $ do
    let executed = show (IntValue 10)
    let expected = "IntValue 10"
    assertEqual "show (IntValue 10)" expected (executed)

testIntValueEq :: Test
testIntValueEq = TestCase $ do
    let executed = (IntValue 10) == (IntValue 10)
    let expected = True
    assertEqual "(IntValue 10) == (IntValue 10)" expected (executed)

testBoolValueShow :: Test
testBoolValueShow = TestCase $ do
    let executed = show (BoolValue True)
    let expected = "BoolValue True"
    assertEqual "show (BoolValue True)" expected (executed)

testBoolValueEq :: Test
testBoolValueEq = TestCase $ do
    let executed = (BoolValue True) == (BoolValue True)
    let expected = True
    assertEqual "(BoolValue True) == (BoolValue True)" expected (executed)

testOperatorShow :: Test
testOperatorShow = TestCase $ do
    let executed = show (Add)
    let expected = "Add"
    assertEqual "show (Add)" expected (executed)

testOperatorEq :: Test
testOperatorEq = TestCase $ do
    let executed = (Add) == (Add)
    let expected = True
    assertEqual "(Add) == (Add)" expected (executed)

testFunctionShow :: Test
testFunctionShow = TestCase $ do
    let executed = show (Function [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])
    let expected = "Function [Push (IntValue 10),Push (IntValue 10),Push (Operator Eq),CallOp,Ret]"
    assertEqual "show (Function [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])" expected (executed)

testFunctionEq :: Test
testFunctionEq = TestCase $ do
    let executed = (Function [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret]) == (Function [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])
    let expected = True
    assertEqual "(Function [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret]) == (Function [Push (IntValue 10), Push (IntValue 10), Push (Operator Eq), CallOp, Ret])" expected (executed)

testStringValueShow :: Test
testStringValueShow = TestCase $ do
    let executed = show (StringValue "Hello World")
    let expected = "StringValue \"Hello World\""
    assertEqual "show (StringValue \"Hello World\")" expected (executed)

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
                    TestLabel "testParseWhiteSpaceFail" testParseWhiteSpaceFail,
                    TestLabel "testSubFailVM" testSubFailVM,
                    TestLabel "testMulFailVM" testMulFailVM,
                    TestLabel "testModFailVM" testModFailVM,
                    TestLabel "testAndFailVM" testAndFailVM,
                    TestLabel "testOrFailVM" testOrFailVM,
                    TestLabel "testNotVM" testNotVM,
                    TestLabel "testEqFailVM" testEqFailVM,
                    TestLabel "testSupFailVM" testSupFailVM,
                    TestLabel "testLessFailVM" testLessFailVM,
                    TestLabel "testConcatFailVM" testConcatFailVM,
                    TestLabel "testLessEqVM" testLessEqVM,
                    TestLabel "testLessEqFailVM" testLessEqFailVM,
                    TestLabel "testSupEqVM" testSupEqVM,
                    TestLabel "testSupEqFailVM" testSupEqFailVM,
                    TestLabel "testNotEqVM" testNotEqVM,
                    TestLabel "testNotEq2VM" testNotEq2VM,
                    TestLabel "testNotEqFailVM" testNotEqFailVM,
                    TestLabel "testListOperationAddVM" testListOperationAddVM,
                    TestLabel "testListOperationSubVM" testListOperationSubVM,
                    TestLabel "testListOperationMulVM" testListOperationMulVM,
                    TestLabel "testListOperationDivVM" testListOperationDivVM,
                    TestLabel "testListOperationModVM" testListOperationModVM,
                    TestLabel "testListOperationAndVM" testListOperationAndVM,
                    TestLabel "testListOperationOrVM" testListOperationOrVM,
                    TestLabel "testListOperationNotVM" testListOperationNotVM,
                    TestLabel "testListOperateNotVM2" testListOperateNotVM2,
                    TestLabel "testListOperationEqVM" testListOperationEqVM,
                    TestLabel "testListOperationLessVM" testListOperationLessVM,
                    TestLabel "testListOperationNotEqVM" testListOperationNotEqVM,
                    TestLabel "testListOperationSupVM" testListOperationSupVM,
                    TestLabel "testListOperationConcatVM" testListOperationConcatVM,
                    TestLabel "testListOperationLessEqVM" testListOperationLessEqVM,
                    TestLabel "testListOperationSupEqVM" testListOperationSupEqVM,
                    TestLabel "testListOperationSubFailVM" testListOperationSubFailVM,
                    TestLabel "testListOperationDivFailVM" testListOperationDivFailVM,
                    TestLabel "testListOperationModFailVM" testListOperationModFailVM,
                    TestLabel "testListOperationNotFailVM" testListOperationNotFailVM,
                    TestLabel "testListOperationEqFailVM" testListOperationEqFailVM,
                    TestLabel "testListOperationLessFailVM" testListOperationLessFailVM,
                    TestLabel "testListOperationNotEqFailVM" testListOperationNotEqFailVM,
                    TestLabel "testListOperationSupFailVM" testListOperationSupFailVM,
                    TestLabel "testListOperationLessEqFailVM" testListOperationLessEqFailVM,
                    TestLabel "testListOperationSupEqFailVM" testListOperationSupEqFailVM,
                    TestLabel "testOperationDivVM" testOperationDivVM,
                    TestLabel "testOperationDivFailVM" testOperationDivFailVM,
                    TestLabel "testOperationModVM" testOperationModVM,
                    TestLabel "testOperationModFailVM" testOperationModFailVM,
                    TestLabel "testOperationDivFailVM2" testOperationDivFailVM2,
                    TestLabel "testOperationEqVM" testOperationEqVM,
                    TestLabel "testOperationEqVM2" testOperationEqVM2,
                    TestLabel "testOperationEqVM3" testOperationEqVM3,
                    TestLabel "testOperatorEqVM3" testOperatorEqVM3,
                    TestLabel "testOperationEqVM4" testOperationEqVM4,
                    TestLabel "testOperationLessVM" testOperationLessVM,
                    TestLabel "testOperationNotVM" testOperationNotVM,
                    TestLabel "testOperationNotVM2" testOperationNotVM2,
                    TestLabel "testOperationOrVM" testOperationOrVM,
                    TestLabel "testOperationOrVM2" testOperationOrVM2,
                    TestLabel "testOperationEqVM5" testOperationEqVM5,
                    TestLabel "testOperationModFailVM2" testOperationModFailVM2,
                    TestLabel "testListOperationDivVM" testListOperationDivVM,
                    TestLabel "testAssignmentFailVM" testAssignmentFailVM,
                    TestLabel "testJumpIfFalseFailVM" testJumpIfFalseFailVM,
                    TestLabel "testJumpIfTrue1VM" testJumpIfTrue1VM,
                    TestLabel "testJumpIfTrue2VM" testJumpIfTrue2VM,
                    TestLabel "testJumpIfTrueFailVM" testJumpIfTrueFailVM,
                    TestLabel "testAssignmentVM3" testAssignmentVM3,
                    TestLabel "testPushArgsOutOfBounds" testPushArgsOutOfBounds,
                    TestLabel "testPushVMEnvFailVM" testPushVMEnvFailVM,
                    TestLabel "testAssignEnvValueFailVM" testAssignEnvValueFailVM,
                    TestLabel "testCallOpFailVM" testCallOpFailVM,
                    TestLabel "testIntValueShow" testIntValueShow,
                    TestLabel "testIntValueEq" testIntValueEq,
                    TestLabel "testBoolValueShow" testBoolValueShow,
                    TestLabel "testBoolValueEq" testBoolValueEq,
                    TestLabel "testOperatorShow" testOperatorShow,
                    TestLabel "testOperatorEq" testOperatorEq,
                    TestLabel "testFunctionShow" testFunctionShow,
                    TestLabel "testFunctionEq" testFunctionEq,
                    TestLabel "testStringValueShow" testStringValueShow,
                    TestLabel "testShowAstInt" testShowAstInt,
                    TestLabel "testShowAstString" testShowAstString,
                    TestLabel "testShowAstBool" testShowAstBool,
                    TestLabel "testShowAstSymbol" testShowAstSymbol,
                    TestLabel "testShowAstMathOp" testShowAstMathOp,
                    TestLabel "testShowAstList" testShowAstList,
                    -- Error handling
                    TestLabel "testFormatError" testFormatError,
                    TestLabel "testFormatError2" testFormatError2,
                    -- Formatter
                    TestLabel "testFormatEnv" testFormatEnv,
                    TestLabel "testFormatEnvOperatorFunctionList" testFormatEnvOperatorFunctionList,
                    TestLabel "testFormatInsts" testFormatInsts
                    -- Interpreter
                    -- TestLabel "testinterpretAST" testinterpretAST
                    -- TestLabel "testInterpretValueInt" testInterpretValueInt,
                    -- TestLabel "testInterpretValueBool" testInterpretValueBool,
                    -- TestLabel "testInterpretValueString" testInterpretValueString,
                    -- TestLabel "testInterpretValueSymbol" testInterpretValueSymbol,
                    -- TestLabel "testInterpretValueError" testInterpretValueError
                ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess
