module CCSAstParserIf where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseIf )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstIf :: Test
testParseAstIf = TestCase $ assertEqual "parseIf" (Just (
    If (AstList [AstSymbol "a", LogicOperator "==", AstSymbol "b"])
    (Define (AstSymbol "x") (AstInt 10)) AstNone,"")) (runParser parseIf "if (a == b) then: let x = 10;"
    )

testParseAstIfElse :: Test
testParseAstIfElse = TestCase $ assertEqual "parseIf" (Just (
    If (AstList [AstSymbol "a", LogicOperator "==", AstSymbol "b"])
    (Define (AstSymbol "x") (AstInt 10))
    (Define (AstSymbol "x") (AstInt 11)),"")) (runParser parseIf "if (a == b) then: let x = 10; else: let x = 11;")


testParseAstIfTernary :: Test
testParseAstIfTernary = TestCase $ assertEqual "parseIf" (Just (
    If (AstList [AstSymbol "a", LogicOperator "==", AstSymbol "b"])
    (Define (AstSymbol "x") (AstInt 10))
    (Define (AstSymbol "x") (AstInt 11)),"")) (runParser parseIf "(a == b) ? let x = 10; : let x = 11;")

testParseAstIfFail :: Test
testParseAstIfFail = TestCase $ assertEqual "parseIfFail" Nothing (runParser parseIf "if (a == b) then let x = 10; else: let x = 11;")
