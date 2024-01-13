module CCSAstParserCall where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseCall )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstCall :: Test
testParseAstCall = TestCase $ assertEqual "parseCall" (Just (
    AstCall (AstSymbol "testFunc")
    (AstList [AstInt 1, AstInt 2, AstString "hello"]),"")) (runParser parseCall "testFunc(1, 2, \"hello\")"
    )

testParseAstCall2 :: Test
testParseAstCall2 = TestCase $ assertEqual "parseCall2" (Just (
    AstCall (AstSymbol "testFunc")
    (AstList [AstInt 1, AstInt 2, AstString "hello"]),"")) (runParser parseCall "testFunc(1, 2, \"hello\");"
    )

testParseAstCallFail :: Test
testParseAstCallFail = TestCase $ assertEqual "parseCallFail" Nothing (runParser parseCall "testFunc(1, 2, \"hello\"")
