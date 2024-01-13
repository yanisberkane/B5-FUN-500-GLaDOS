module CCSAstParserList where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstList )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstList :: Test
testParseAstList = TestCase $ assertEqual "parseAstList" (Just (AstList [
        AstInt 1, AstSymbol "testSymbol", AstString "testString", AstBool True, Define (AstSymbol "varName") (AstInt 1), Assign (AstSymbol "name") (AstInt 1), AstList [], AstList [Define (AstSymbol "x") (AstInt 10)], AstOperator "+", LogicOperator "&&"
    ],"")) (runParser parseAstList "(1 testSymbol \"testString\" True let varName = 1; name = 1; () {let x = 10;} + &&)")

testParseAstListFail :: Test
testParseAstListFail = TestCase $ assertEqual "parseAstListFail" Nothing (runParser parseAstList "(1 testSymbol \"testString\" True let varName = 1; varName = 1; () {let x = 10;} + &&")
