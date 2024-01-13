module CCSAstParserDefine where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseDefine )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstDefineLet :: Test
testParseAstDefineLet = TestCase $ assertEqual "parseDefineLet" (Just (Define (AstSymbol "varName") (AstInt 1),"")) (runParser parseDefine "let varName = 1;")

testParseAstDefineVar :: Test
testParseAstDefineVar = TestCase $ assertEqual "parseDefineVar" (Just (Define (AstSymbol "varName") (AstInt 1),"")) (runParser parseDefine "var varName = 1;")

testParseAstDefineLetWithSpaces :: Test
testParseAstDefineLetWithSpaces = TestCase $ assertEqual "parseDefineLetWithSpaces" (Just (Define (AstSymbol "varName") (AstInt 1),"")) (runParser parseDefine "let      varName    = 1;")

testParseAstDefineVarWithSpaces :: Test
testParseAstDefineVarWithSpaces = TestCase $ assertEqual "parseDefineVarWithSpaces" (Just (Define (AstSymbol "varName") (AstInt 1),"")) (runParser parseDefine "var      varName    = 1;")

testParseAstDefineFail :: Test
testParseAstDefineFail = TestCase $ assertEqual "parseDefineFail" Nothing (runParser parseDefine "let varName = 1")

testParseAstDefineFail2 :: Test
testParseAstDefineFail2 = TestCase $ assertEqual "parseDefineFail2" Nothing (runParser parseDefine "var varName = 1")

testParseAstDefineFail3 :: Test
testParseAstDefineFail3 = TestCase $ assertEqual "parseDefineFail3" Nothing (runParser parseDefine "let varName = =;")

testParseAstDefineFail4 :: Test
testParseAstDefineFail4 = TestCase $ assertEqual "parseDefineFail4" Nothing (runParser parseDefine "var varName = =;")
