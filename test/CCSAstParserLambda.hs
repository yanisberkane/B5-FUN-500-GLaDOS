module CCSAstParserLambda where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseLambda )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstLambda :: Test
testParseAstLambda = TestCase $ assertEqual "parseLambda" (Just (Lambda (AstList[AstSymbol "x"]) (AstList [AstMathOp (AstSymbol "x") (AstOperator "+") (AstInt 1)]),"")) (runParser parseLambda "(x) => {x + 1}")

testParseAstLambda2 :: Test
testParseAstLambda2 = TestCase $ assertEqual "parseLambda2" (Just (Lambda (AstList[AstSymbol "x", AstSymbol "y"]) (
    AstList [
        AstMathOp (AstSymbol "x") (AstOperator "+") (AstSymbol "y")
    ]),"" ))
    (runParser parseLambda "(x, y) => {x + y}")

testParseAstLambda3 :: Test
testParseAstLambda3 = TestCase $ assertEqual "parseLambda3" (Just (Lambda (AstList[AstSymbol "argOne", AstSymbol "argTwo"]) (
    AstList [
        Define (AstSymbol "x") (AstInt 5),
        Define (AstSymbol "b") (AstInt 4)
    ]),"" ))
    (runParser parseLambda "(argOne, argTwo) => {let x = 5; let b = 4;}")
