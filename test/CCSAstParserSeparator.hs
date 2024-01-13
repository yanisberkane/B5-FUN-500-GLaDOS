module CCSAstParserSeparator where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseSeparator )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstSeparator  :: Test
testParseAstSeparator = TestCase $ assertEqual "parseAstSeparator" (Just (Separator ';',"")) (runParser parseSeparator ";")

testParseAstSeparatorWithSpaces  :: Test
testParseAstSeparatorWithSpaces = TestCase $ assertEqual "parseAstSeparator" (Just (Separator ';',"")) (runParser parseSeparator "      ;")

testParseAstSeparatorFail  :: Test
testParseAstSeparatorFail = TestCase $ assertEqual "parseAstSeparatorFail" Nothing (runParser parseSeparator "nothing")
