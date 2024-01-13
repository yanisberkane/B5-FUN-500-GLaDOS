module CCSAstParserMathOperators where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstOperator )
import Parser ( Parser(runParser) )
import Types (Ast(..))
testParseAstOperatorAdd :: Test
testParseAstOperatorAdd = TestCase $ assertEqual "parseAstOperatorAdd" (Just (AstOperator "+","")) (runParser parseAstOperator "+")

testParseAstOperatorSub :: Test
testParseAstOperatorSub = TestCase $ assertEqual "parseAstOperatorSub" (Just (AstOperator "-","")) (runParser parseAstOperator "-")

testParseAstOperatorMul :: Test
testParseAstOperatorMul = TestCase $ assertEqual "parseAstOperatorMul" (Just (AstOperator "*","")) (runParser parseAstOperator "*")

testParseAstOperatorDiv :: Test
testParseAstOperatorDiv = TestCase $ assertEqual "parseAstOperatorDiv" (Just (AstOperator "/","")) (runParser parseAstOperator "/")

testParseAstOperatorMod :: Test
testParseAstOperatorMod = TestCase $ assertEqual "parseAstOperatorMod" (Just (AstOperator "%","")) (runParser parseAstOperator "%")
