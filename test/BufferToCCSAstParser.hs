module BufferToCCSAstParser where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( bufferToCCSAst )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseBufferToCCSAst :: Test
testParseBufferToCCSAst = TestCase $ assertEqual "parseBufferToCCSAst" (Just ([AstInt 1])) (bufferToCCSAst "1")

testParseBufferToCCSAst2 :: Test
testParseBufferToCCSAst2 = TestCase $ assertEqual "parseBufferToCCSAst" (
    Just ([AstInt 1, AstSymbol "hello", AstString "string test", AstBool True, AstBool False]))
    (bufferToCCSAst "      1 hello \"string test\" TrueFalse")

testParseBufferToCCSAstFail :: Test
testParseBufferToCCSAstFail = TestCase $ assertEqual "parseBufferToCCSAst" (Just []) (bufferToCCSAst "=")