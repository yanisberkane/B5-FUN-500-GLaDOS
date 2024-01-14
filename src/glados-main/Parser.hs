{-# LANGUAGE LambdaCase #-}
module Parser (
    -- *Parser
    -- $parser
    Parser(..),
    -- *Parser functions
    -- $parserfunctions
    parseChar,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseSymbol,
    parseQuotedSymbol,
    parseWhiteSpace,
    parseString,
    parseBool,
    parseOperator,
    parseLogicOperator,
    parseNoneOf) where
import System.IO
import Data.Maybe
import Control.Applicative
import qualified Data.Functor

{- $parser
    This module contains the parser used in the Glados interpreter.

    Parser is the type used to represent the parser.

-}

{- $parserfunctions
    This module contains the functions used to parse the Glados language.
-}

-- |Parser is the type used to represent the parser.
newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

-- |Parser is an instance of Functor.
instance Functor Parser where
    fmap fct p = Parser $ \str -> case runParser p str of
        Just (x, xs) -> Just (fct x, xs)
        Nothing -> Nothing

-- |Parser is an instance of Applicative.
instance Applicative Parser where
    pure x = Parser $ \str -> Just (x, str)
    p1 <*> p2 = Parser $ \str -> case runParser p1 str of
        Just (fct, xs) -> case runParser p2 xs of
            Just (x, ys) -> Just (fct x, ys)
            Nothing -> Nothing
        Nothing -> Nothing

-- |Parser is an instance of Alternative.
instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \str -> case runParser p1 str of
        Just (x, xs) -> Just (x, xs)
        Nothing -> runParser p2 str

-- |Parser is an instance of Monad.
instance Monad Parser where
    return = pure
    p >>= fct = Parser $ \str -> case runParser p str of
        Just (x, xs) -> runParser (fct x) xs
        Nothing -> Nothing

-- |parseChar parses a single character and returns it.
parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
    (x:xs) | c == x -> Just (c, xs)
    _ -> Nothing

-- |parseAnyChar parses any character in a given string and returns it.
parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \case
    (x:xs) | x `elem` str -> Just (x, xs)
    _ -> Nothing

-- |parseOr parses either of two parsers and returns the result of the first one that succeeds.
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ \str -> runParser p1 str <|> runParser p2 str

-- |parseAnd parses two parsers and returns a tuple of the results.
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \str -> case runParser p1 str of
    Just (x, xs) -> case runParser p2 xs of
        Just (y, ys) -> Just ((x, y), ys)
        Nothing -> Nothing
    Nothing -> Nothing

-- |parseAndWith parses two parsers and returns the result of a function applied to the results.
parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \str -> case runParser p1 str of
    Just (x, xs) -> case runParser p2 xs of
        Just (y, ys) -> Just (f x y, ys)
        Nothing -> Nothing
    Nothing -> Nothing

-- |parseMany parses a parser zero or more times and returns a list of the results.
parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \str -> case runParser p str of
    Just (x, xs) -> case runParser (parseMany p) xs of
        Just (y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Just ([], str)

-- |parseSome parses a parser one or more times and returns a list of the results.
parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

-- |parseNoneOf parses any character not present in a given string and returns it.
parseNoneOf :: String -> Parser Char
parseNoneOf str = Parser $ \case
    (x:xs) | x `notElem` str -> Just (x, xs)
    _ -> Nothing

-- |parseUInt parses an unsigned integer and returns it.
parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar "0123456789")

-- |parseAstInt parses an integer and returns it.
parseOperator :: Parser String
parseOperator = parseSome $ parseAnyChar "+-*/%"

-- |parseAstInt parses an integer and returns it.
parseInt :: Parser Int
parseInt = (\x y -> if x == '-' then -y else y) <$> parseOr (parseChar '-') (parseChar '+') <*> parseUInt

-- |parseLogicOperator parses a logical operator and returns it.
parseLogicOperator :: Parser String
parseLogicOperator = parseOr (parseString "&&") (parseString "||")
                 <|> parseOr (parseString "==") (parseString "!=")
                 <|> parseOr (parseString "<=") (parseString ">=")
                 <|> parseOr (parseString "<") (parseString ">")
                 <|> parseString "!"

-- |parseQuotedSymbol parses a quoted symbol (aka a string) and returns it.
parseQuotedSymbol :: Parser String
parseQuotedSymbol = parseChar '\"' *> parseSome (parseNoneOf "\"") <* parseChar '\"'

-- |parseSymbol parses a symbol and returns it.
parseSymbol :: Parser String
parseSymbol = parseSome $ parseOr (parseAnyChar ['a'..'z']) (parseAnyChar ['A'..'Z'])

-- |parseBool parses a boolean and returns it.
parseBool :: Parser Bool
parseBool = parseOr (parseString "True"  Data.Functor.$> True)
                    (parseString "False" Data.Functor.$> False)

-- |parseWhiteSpace parses whitespace new-line and tab and returns it.
parseWhiteSpace :: Parser String
parseWhiteSpace = parseSome $ parseAnyChar " \t\n"

-- |parseString parses given string and returns it.
parseString :: String -> Parser String
parseString = traverse parseChar
