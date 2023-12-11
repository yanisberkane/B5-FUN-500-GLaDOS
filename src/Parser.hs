module Parser (parseInput) where

import System.IO
import Data.Maybe
import qualified Data.Text as T

type Parser a = String -> Maybe (a , String )

parseChar :: Char -> Parser Char
parseChar c (x:xs) | c == x = Just (c, xs)
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar str (x:xs) | x `elem` str = Just (x, xs)
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str | isNothing(p1 str) = p2 str
parseOr p1 _ str = p1 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 str = case p1 str of
    Just (x, xs) -> case p2 xs of
        Just (y, ys) -> Just ((x, y), ys)
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 str = case p1 str of
    Just (x, xs) -> case p2 xs of
        Just (y, ys) -> Just ((f x y), ys)
        Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p str = case p str of
    Just (x, xs) -> case parseMany p xs of
        Just (y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p str = case p str of
    Just (x, xs) -> case parseMany p xs of
        Just (y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Nothing

parseUInt :: Parser Int
parseUInt str = case parseSome (parseAnyChar "0123456789") str of
    Just (x, xs) -> Just (read x, xs)
    Nothing -> case parseChar '+' str of
        Just (_, xs) -> parseUInt xs
        Nothing -> Nothing

parseInt :: Parser Int
parseInt str = case parseOr (parseChar '-') (parseChar '+') str of
    Just (x, xs) -> case parseUInt xs of
        Just (y, ys) -> Just (if x == '-' then -y else y, ys)
        Nothing -> Nothing
    Nothing -> parseUInt str

parseInput :: String -> IO ()
parseInput filename = do
    fileHandle <- openFile filename ReadMode
    contents <- hGetContents fileHandle
    print contents
    hClose fileHandle