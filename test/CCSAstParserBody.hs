module CCSAstParserBody where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseBody )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstBody :: Test
testParseAstBody = TestCase $ assertEqual "parseBody" (Just (AstList[Define (AstSymbol "x") (AstInt 1)],"")) (runParser parseBody "{let x = 1;}")

testParseAstBodyWithSpaces :: Test
testParseAstBodyWithSpaces = TestCase $ assertEqual "parseBody" (Just (AstList[Define (AstSymbol "x") (AstInt 1)],"")) (runParser parseBody "{   let x = 1;   }")

testParseAstBodyFail :: Test
testParseAstBodyFail = TestCase $ assertEqual "parseBody" Nothing (runParser parseBody "{let x = 1;")