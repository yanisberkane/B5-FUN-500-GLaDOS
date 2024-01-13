module CCSAstParserArgList where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseArgList )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstArgList :: Test
testParseAstArgList = TestCase $ assertEqual "parseArgList" (Just (AstList [AstInt 1, AstInt 2, AstString "hello"],"")) (runParser parseArgList "(1, 2, \"hello\")")

testParseAstArgListWithSpaces :: Test
testParseAstArgListWithSpaces = TestCase $ assertEqual "parseArgList" (Just (AstList [AstInt 1, AstInt 2, AstString "hello"],"")) (runParser parseArgList "   (1,  2, \"hello\")")

testParseAstArgListFail :: Test
testParseAstArgListFail = TestCase $ assertEqual "parseArgList" Nothing (runParser parseArgList "(1, 2 , \"hello\"")