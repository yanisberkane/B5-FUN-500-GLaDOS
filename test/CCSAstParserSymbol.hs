module CCSAstParserSymbol where
import Test.HUnit ( assertEqual, Test(TestCase) )
import CCSAstParser ( parseAstSymbol )
import Parser ( Parser(runParser) )
import Types (Ast(..))

testParseAstSymbol  :: Test
testParseAstSymbol = TestCase $ assertEqual "parseAstSymbol" (Just (AstSymbol "aSymbol","")) (runParser parseAstSymbol "aSymbol")

testParseAstSymbolWithSpaces  :: Test
testParseAstSymbolWithSpaces = TestCase $ assertEqual "parseAstSymbol" (Just (AstSymbol "aSymbol","")) (runParser parseAstSymbol "      aSymbol")

testParseAstSymbolWithNumber  :: Test
testParseAstSymbolWithNumber = TestCase $ assertEqual "parseAstSymbolWithNumber" (Just (AstSymbol "aSymbol","1")) (runParser parseAstSymbol "aSymbol1")

testParseAstSymbolWithNumberAndUnderscore  :: Test
testParseAstSymbolWithNumberAndUnderscore = TestCase $ assertEqual "parseAstSymbolWithNumberAndUnderscore" (Just (AstSymbol "aSymbol","_1")) (runParser parseAstSymbol "aSymbol_1")

testParseAstSymbolFail  :: Test
testParseAstSymbolFail = TestCase $ assertEqual "parseAstSymbolFail" Nothing (runParser parseAstSymbol "1Symbol")

testParseAstSymbolFail2  :: Test
testParseAstSymbolFail2 = TestCase $ assertEqual "parseAstSymbolFail2" Nothing (runParser parseAstSymbol "_=Symbol1")