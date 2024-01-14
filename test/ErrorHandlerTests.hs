module ErrorHandlerTests where
import ErrorHandler
import System.Exit (ExitCode(ExitFailure))
import Test.HUnit ( assertEqual, Test(TestCase) )

testFormatError :: Test
testFormatError = TestCase $ do
    let expected = "*** ERROR: ParsingError \"foo\""
    assertEqual "formatError (ParsingError \"foo\")" expected (formatError (ParsingError "foo"))

testFormatError2 :: Test
testFormatError2 = TestCase $ do
    let expected = "*** ERROR: variable foo is not bound."
    assertEqual "formatError (UndefinedVariableError \"foo\")" expected (formatError (UndefinedVariableError "foo"))