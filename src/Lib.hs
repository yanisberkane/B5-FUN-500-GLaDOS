module Lib
    ( getSSymbol
    , getSInt
    , getSList
    ) where

import Data.Maybe

data SExpr = SInt Int
            | SSymbol String
            | SList [SExpr]
            deriving Show

data Ast = Define { cat :: SExpr, name :: SExpr, value :: SExpr }
         | AstInt Int
         | AstSymbol String
         deriving Show

getSSymbol :: SExpr -> Maybe String
getSSymbol (SSymbol str) = Just str
getSSymbol _ = Nothing

getSInt :: SExpr -> Maybe Int
getSInt (SInt int) = Just int
getSInt _ = Nothing

getSList :: SExpr -> Maybe [SExpr]
getSList (SList list) = Just list
getSList _ = Nothing

sexprToAst :: SExpr -> Maybe Ast
sexprToAst (SSymbol str) = Just $ AstSymbol str
sexprToAst (SInt int) = Just $ AstInt int
sexprToAst (SList list) = Just $ Define (head list) (list !! 1) (list !! 2)

printTree :: SExpr -> Maybe String
printTree (SSymbol str) = Just $ "a Symbol '" ++ str ++ "'"
printTree (SInt int) = Just $ "a Number " ++ show int
printTree (SList list) = Just $ "a List with" ++ foldl appendItem "" list
  where
    appendItem acc x = acc ++ " " ++ fromJust (printTree x)