module CCSAstParserBool where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstBool )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstBoolTrue :: Test
testParseAstBoolTrue = TestCase $ assertEqual "parseAstBool" (Just (AstBool True,"")) (runParser parseAstBool "True")

testParseAstBoolFalse :: Test
testParseAstBoolFalse = TestCase $ assertEqual "parseAstBool" (Just (AstBool False,"")) (runParser parseAstBool "False")

testParseAstBoolFail :: Test
testParseAstBoolFail = TestCase $ assertEqual "parseAstBool" Nothing (runParser parseAstBool "true")

testParseAstBoolFail2 :: Test
testParseAstBoolFail2 = TestCase $ assertEqual "parseAstBool" Nothing (runParser parseAstBool "false")