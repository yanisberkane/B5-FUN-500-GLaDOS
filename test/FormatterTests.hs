module FormatterTests where
import Formatter
import Test.HUnit ( assertEqual, Test(TestCase) )
import VMTypes

testFormatEnv :: Test
testFormatEnv = TestCase $ do
    let expected = "foo = INT 1\nbar = BOOL True\nbaz = STRING hello\n"
    assertEqual "formatEnv [(\"foo\", IntValue 1), (\"bar\", BoolValue True), (\"baz\", StringValue \"hello\")]" expected (formatEnv [("foo", IntValue 1), ("bar", BoolValue True), ("baz", StringValue "hello")])

testFormatEnvOperatorFunctionList :: Test
testFormatEnvOperatorFunctionList = TestCase $ do
    let expected = "foo = OPERATOR Add\nbar = FUNCTION [Push (IntValue 1),Push (IntValue 2)]\nbaz = LIST [IntValue 1,BoolValue True,StringValue \"hello\"]\n"
    assertEqual "formatEnv [(\"foo\", Operator Add), (\"bar\", Function [Push (IntValue 1), Push (IntValue 2)]), (\"baz\", ListValue [IntValue 1, BoolValue True, StringValue \"hello\"])]" expected (formatEnv [("foo", Operator Add), ("bar", Function [Push (IntValue 1), Push (IntValue 2)]), ("baz", ListValue [IntValue 1, BoolValue True, StringValue "hello"])])

testFormatInsts :: Test
testFormatInsts = TestCase $ do
    let expected = "Push (IntValue 1)\nPush (IntValue 2)\nPush (Operator Add)\nCallOp\n"
    assertEqual "formatInsts [Push (IntValue 1), Push (IntValue 2), Add]" expected (formatInsts [Push (IntValue 1), Push (IntValue 2), Push (Operator Add), CallOp])