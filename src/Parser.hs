module Parser (parseInput) where

import System.IO
import qualified Data.Text as T

type Parser a = String -> Maybe (a , String )

parseChar :: Char -> Parser Char
parseChar c (x:xs) | c == x = Just (c, xs)
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar str (x:xs) | x `elem` str = Just (x, xs)
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str = case p1 str of
    Just (x, xs) -> Just (x, xs)
    Nothing -> p2 str

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

parseInput :: String -> IO ()
parseInput filename = do
    fileHandle <- openFile filename ReadMode
    contents <- hGetContents fileHandle
    print contents
    hClose fileHandle