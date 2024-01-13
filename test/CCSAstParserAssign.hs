module CCSAstParserAssign where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAssign )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstAssignInt :: Test
testParseAstAssignInt = TestCase $ assertEqual "parseAssignInt" (Just (Assign (AstSymbol "varName") (AstInt 1),"")) (runParser parseAssign "varName = 1;")

testParseAstAssignString :: Test
testParseAstAssignString = TestCase $ assertEqual "parseAssignString" (Just (Assign (AstSymbol "varName") (AstString "a test string"),"")) (runParser parseAssign "varName = \"a test string\";")

testParseAstAssignBool :: Test
testParseAstAssignBool = TestCase $ assertEqual "parseAssignBool" (Just (Assign (AstSymbol "varName") (AstBool True),"")) (runParser parseAssign "varName = True;")

testParseAstAssignBool2 :: Test
testParseAstAssignBool2 = TestCase $ assertEqual "parseAssignBool" (Just (Assign (AstSymbol "varName") (AstBool False),"")) (runParser parseAssign "varName = False;")

testParseAstAssignWithSpaces :: Test
testParseAstAssignWithSpaces = TestCase $ assertEqual "parseAssign" (Just (Assign (AstSymbol "varName") (AstInt 1),"")) (runParser parseAssign "      varName    = 1;")

testParseAstAssignFail :: Test
testParseAstAssignFail = TestCase $ assertEqual "parseAssign" Nothing (runParser parseAssign "varName = 1")

