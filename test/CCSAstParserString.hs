module CCSAstParserString where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstString )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstString :: Test
testParseAstString = TestCase $ assertEqual "parseAstString" (Just (AstString "a test string","")) (runParser parseAstString "\"a test string\"")

testParseAstString2 :: Test
testParseAstString2 = TestCase $ assertEqual "parseAstString" (Just (AstString "a_test_string123FF","")) (runParser parseAstString "\"a_test_string123FF\"")

testParseAstStringWithSpaces :: Test
testParseAstStringWithSpaces = TestCase $ assertEqual "parseAstString" (Just (AstString "a test string","")) (runParser parseAstString "      \"a test string\"")

testParseAstStringWithNumber :: Test
testParseAstStringWithNumber = TestCase $ assertEqual "parseAstStringWithNumber" (Just (AstString "a test string","1")) (runParser parseAstString "\"a test string\"1")

testParseAstStringFail :: Test
testParseAstStringFail = TestCase $ assertEqual "parseAstStringFail" Nothing (runParser parseAstString "nothing")