module CCSAstParserNamedCall where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseNamedFunc )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstNamedCall :: Test
testParseAstNamedCall = TestCase $ assertEqual "parseNamedCall"
    (Just (NamedCall (AstSymbol "test") (Lambda (AstList[AstSymbol "x"])
    (AstList [AstMathOp (AstSymbol "x") (AstOperator "+") (AstInt 1)])),"")) (runParser parseNamedFunc "test(x) => {x + 1};")

testParseAstNamedCall2 :: Test
testParseAstNamedCall2 = TestCase $ assertEqual "parseNamedCall2" (Just (NamedCall (AstSymbol "testTwo") (Lambda (AstList[AstSymbol "x", AstSymbol "y"]) (
    AstList [
        AstMathOp (AstSymbol "x") (AstOperator "+") (AstSymbol "y")
    ])),"" ))
    (runParser parseNamedFunc "testTwo(x, y) => {x + y};")

testParseAstNamedCall3 :: Test
testParseAstNamedCall3 = TestCase $ assertEqual "parseNamedCall3" (Just (NamedCall (AstSymbol "testThree") (Lambda (AstList[AstSymbol "argOne", AstSymbol "argTwo"]) (
    AstList [
        Define (AstSymbol "x") (AstInt 5),
        Define (AstSymbol "b") (AstInt 4)
    ])),"" ))
    (runParser parseNamedFunc "testThree (argOne, argTwo) => {let x = 5; let b = 4;};")
