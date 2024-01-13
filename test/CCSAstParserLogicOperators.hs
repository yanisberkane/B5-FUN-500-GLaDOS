module CCSAstParserLogicOperators where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstLogicOperator )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstLogicOperatorAnd :: Test
testParseAstLogicOperatorAnd = TestCase $ assertEqual "parseAstLogicOperatorAnd" (Just (LogicOperator "&&","")) (runParser parseAstLogicOperator "&&")

testParseAstLogicOperatorOr :: Test
testParseAstLogicOperatorOr = TestCase $ assertEqual "parseAstLogicOperatorOr" (Just (LogicOperator "||","")) (runParser parseAstLogicOperator "||")

testParseAstLogicOperatorEqual :: Test
testParseAstLogicOperatorEqual = TestCase $ assertEqual "parseAstLogicOperatorEqual" (Just (LogicOperator "==","")) (runParser parseAstLogicOperator "==")

testParseAstLogicOperatorNotEqual :: Test
testParseAstLogicOperatorNotEqual = TestCase $ assertEqual "parseAstLogicOperatorNotEqual" (Just (LogicOperator "!=","")) (runParser parseAstLogicOperator "!=")

testParseAstLogicOperatorLessThan :: Test
testParseAstLogicOperatorLessThan = TestCase $ assertEqual "parseAstLogicOperatorLessThan" (Just (LogicOperator "<","")) (runParser parseAstLogicOperator "<")

testParseAstLogicOperatorLessThanOrEqual :: Test
testParseAstLogicOperatorLessThanOrEqual = TestCase $ assertEqual "parseAstLogicOperatorLessThanOrEqual" (Just (LogicOperator "<=","")) (runParser parseAstLogicOperator "<=")

testParseAstLogicOperatorGreaterThan :: Test
testParseAstLogicOperatorGreaterThan = TestCase $ assertEqual "parseAstLogicOperatorGreaterThan" (Just (LogicOperator ">","")) (runParser parseAstLogicOperator ">")

testParseAstLogicOperatorGreaterThanOrEqual :: Test
testParseAstLogicOperatorGreaterThanOrEqual = TestCase $ assertEqual "parseAstLogicOperatorGreaterThanOrEqual" (Just (LogicOperator ">=","")) (runParser parseAstLogicOperator ">=")

testParseAstLogicOperatorNot :: Test
testParseAstLogicOperatorNot = TestCase $ assertEqual "parseAstLogicOperatorNot" (Just (LogicOperator "!","")) (runParser parseAstLogicOperator "!")
