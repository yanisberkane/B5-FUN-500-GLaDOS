module ParserTests where
import Test.HUnit ( assertEqual, Test(TestCase) )
import Control.Applicative
import qualified Data.Functor
import Data.Maybe
import Parser

testAlternative :: Test
testAlternative = TestCase $ do
    let parser = runParser (parseChar 'a' <|> parseChar 'b') "abcd"
    let expected = Just ('a', "bcd")
    assertEqual "parseChar 'a' <|> parseChar 'b' on 'abcd'" expected parser

testEmptyAlternative :: Test
testEmptyAlternative = TestCase $ do
    let parser = runParser (parseChar 'a' <|> parseChar 'b' <|> empty) "cdef"
    let expected = Nothing
    assertEqual "parseChar 'a' <|> parseChar 'b' on 'cdef'" expected parser

testParseChar :: Test
testParseChar = TestCase $ do
    let parser = runParser (parseChar 'a') "abcd"
    let expected = Just ('a', "bcd")
    assertEqual "parseChar 'a' on 'abcd'" expected parser

testParseCharFail :: Test
testParseCharFail = TestCase $ do
    let parser = runParser (parseChar 'a') "bcde"
    let expected = Nothing
    assertEqual "parseChar 'a' on 'bcde'" expected parser

testParseAnyChar :: Test
testParseAnyChar = TestCase $ do
    let parser = runParser (parseAnyChar "bca") "cdef"
    let expected = Just ('c', "def")
    assertEqual "parseAnyChar \"bca\" on \"cdef\"" expected parser

testParseAnyCharFail :: Test
testParseAnyCharFail = TestCase $ do
    let parser = runParser (parseAnyChar "bca") "defg"
    let expected = Nothing
    assertEqual "parseAnyChar \"bca\" on \"defg\"" expected parser

testParseOr :: Test
testParseOr = TestCase $ do
    let parser = runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
    let expected = Just ('a', "bcd")
    assertEqual "parseOr (parseChar 'a') (parseChar 'b') on 'abcd'" expected parser

testParseOr2 :: Test
testParseOr2 = TestCase $ do
    let parser = runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcde"
    let expected = Just ('b', "cde")
    assertEqual "parseOr (parseChar 'a') (parseChar 'b') on 'bcde'" expected parser

testParseAnd :: Test
testParseAnd = TestCase $ do
    let parser = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
    let expected = Just (('a', 'b'), "cd")
    assertEqual "parseAnd (parseChar 'a') (parseChar 'b') on 'abcd'" expected parser

testParseAndFail :: Test
testParseAndFail = TestCase $ do
    let parser = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acde"
    let expected = Nothing
    assertEqual "parseAnd (parseChar 'a') (parseChar 'b') on 'acde'" expected parser

testParseAndFail2 :: Test
testParseAndFail2 = TestCase $ do
    let parser = runParser (parseAnd (parseChar 'z') (parseChar 'b')) "bcde"
    let expected = Nothing
    assertEqual "parseAnd (parseChar 'a') (parseChar 'b') on 'bcde'" expected parser

testParseAndWith :: Test
testParseAndWith = TestCase $ do
    let parser = runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd"
    let expected = Just ("ab", "cd")
    assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') on 'abcd'" expected parser

testParseAndWithFail :: Test
testParseAndWithFail = TestCase $ do
    let parser = runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "acde"
    let expected = Nothing
    assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') on 'acde'" expected parser

testParseAndWithFail2 :: Test
testParseAndWithFail2 = TestCase $ do
    let parser = runParser (parseAndWith (\x y -> [x, y]) (parseChar 'z') (parseChar 'b')) "bcde"
    let expected = Nothing
    assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') on 'bcde'" expected parser

testParseMany :: Test
testParseMany = TestCase $ do
    let parser = runParser (parseMany (parseChar 'a')) "aaabcd"
    let expected = Just ("aaa", "bcd")
    assertEqual "parseMany (parseChar 'a') on 'aaabcd'" expected parser

testParseManyFail :: Test
testParseManyFail = TestCase $ do
    let parser = runParser (parseMany (parseChar 'a')) "bcde"
    let expected = Just ("", "bcde")
    assertEqual "parseMany (parseChar 'a') on 'bcde'" expected parser

testParseManyFail2 :: Test
testParseManyFail2 = TestCase $ do
    let parser = runParser (parseMany (parseChar ' ')) ""
    let expected = Just ("", "")
    assertEqual "parseMany (parseChar ' ') on ''" expected parser

testParseManyFail3 :: Test
testParseManyFail3 = TestCase $ do
    let parser = runParser (parseMany (parseChar ' ')) "abc"
    let expected = Just ("", "abc")
    assertEqual "parseMany (parseChar ' ') on 'abc'" expected parser

testParseSome :: Test
testParseSome = TestCase $ do
    let parser = runParser (parseSome (parseChar 'a')) "aaabcd"
    let expected = Just ("aaa", "bcd")
    assertEqual "parseSome (parseChar 'a') on 'aaabcd'" expected parser

testParseSomeFail :: Test
testParseSomeFail = TestCase $ do
    let parser = runParser (parseSome (parseChar 'a')) "bcde"
    let expected = Nothing
    assertEqual "parseSome (parseChar 'a') on 'bcde'" expected parser

testParseSomeFail2 :: Test
testParseSomeFail2 = TestCase $ do
    let parser = runParser (parseSome (parseChar 'z')) "testMessage"
    let expected = Nothing
    assertEqual "parseSome (parseChar 'z') on 'testMessage'" expected parser

testParseNoneOf :: Test
testParseNoneOf = TestCase $ do
    let parser = runParser (parseNoneOf "abc") "defg"
    let expected = Just ('d', "efg")
    assertEqual "parseNoneOf \"abc\" on \"defg\"" expected parser

testParseNoneOfFail :: Test
testParseNoneOfFail = TestCase $ do
    let parser = runParser (parseNoneOf "abc") "abc"
    let expected = Nothing
    assertEqual "parseNoneOf \"abc\" on \"abc\"" expected parser

testParseUInt :: Test
testParseUInt = TestCase $ do
    let parser = runParser parseUInt "1234"
    let expected = Just (1234, "")
    assertEqual "parseUInt on \"1234\"" expected parser

testParseUIntFail :: Test
testParseUIntFail = TestCase $ do
    let parser = runParser parseUInt "abc"
    let expected = Nothing
    assertEqual "parseUInt on \"abc\"" expected parser

testParseInt :: Test
testParseInt = TestCase $ do
    let parser = runParser parseInt "-1234"
    let expected = Just (-1234, "")
    assertEqual "parseInt on \"-1234\"" expected parser

testParseInt2 :: Test
testParseInt2 = TestCase $ do
    let parser = runParser parseInt "+1234"
    let expected = Just (1234, "")
    assertEqual "parseInt on \"+1234\"" expected parser

testParseIntFail :: Test
testParseIntFail = TestCase $ do
    let parser = runParser parseInt "abc"
    let expected = Nothing
    assertEqual "parseInt on \"abc\"" expected parser

testParseQuotedSymbol :: Test
testParseQuotedSymbol = TestCase $ do
    let parser = runParser parseQuotedSymbol "\"foo\""
    let expected = Just ("foo", "")
    assertEqual "parseQuotedSymbol on \"foo\"" expected parser

testParseQuotedSymbolFail :: Test
testParseQuotedSymbolFail = TestCase $ do
    let parser = runParser parseQuotedSymbol "foo"
    let expected = Nothing
    assertEqual "parseQuotedSymbol on \"foo\"" expected parser

testParseSymbol :: Test
testParseSymbol = TestCase $ do
    let parser = runParser parseSymbol "foo"
    let expected = Just ("foo", "")
    assertEqual "parseSymbol on \"foo\"" expected parser

testParseSymbolFail :: Test
testParseSymbolFail = TestCase $ do
    let parser = runParser parseSymbol "\"foo\""
    let expected = Nothing
    assertEqual "parseSymbol on \"foo\"" expected parser

testParseWhiteSpace :: Test
testParseWhiteSpace = TestCase $ do
    let parser = runParser parseWhiteSpace "  foo"
    let expected = Just ("  ", "foo")
    assertEqual "parseWhiteSpace on \"  foo\"" expected parser

testParseWhiteSpaceFail :: Test
testParseWhiteSpaceFail = TestCase $ do
    let parser = runParser parseWhiteSpace "foo"
    let expected = Nothing
    assertEqual "parseWhiteSpace on \"foo\"" expected parser