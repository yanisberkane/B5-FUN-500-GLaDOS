module CCSAstParserInt where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstInt )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstInt :: Test
testParseAstInt = TestCase $ assertEqual "parseAstInt" (Just (AstInt 1,"")) (runParser parseAstInt "1")

testParseAstIntWithSpaces :: Test
testParseAstIntWithSpaces = TestCase $ assertEqual "parseAstInt" (Just (AstInt 1,"")) (runParser parseAstInt "      1")

testParseAstIntNegative :: Test
testParseAstIntNegative = TestCase $ assertEqual "parseAstInt" (Just (AstInt (-1),"")) (runParser parseAstInt "-1")

testParseAstIntFail :: Test
testParseAstIntFail = TestCase $ assertEqual "parseAstInt" Nothing (runParser parseAstInt "a1")

testParseAstIntFail2 :: Test
testParseAstIntFail2 = TestCase $ assertEqual "parseAstInt" Nothing (runParser parseAstInt "_1")