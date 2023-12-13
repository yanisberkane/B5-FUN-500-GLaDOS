module Types where

import qualified Data.Map as Map
import Data.Maybe

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show, Eq)

data Ast = Define SExpr SExpr Ast
         | AstInt Int
         | AstSymbol String
         | AstList [Ast]
         deriving (Show, Eq)

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
sexprToAst (SList list) = AstList <$> mapM sexprToAst list

printTree :: SExpr -> Maybe String
printTree (SSymbol str) = Just $ "a Symbol '" ++ str ++ "'"
printTree (SInt int) = Just $ "a Number " ++ show int
printTree (SList list) = Just $ "a List with" ++ foldl appendItem "" list
  where
    appendItem acc x = acc ++ " " ++ fromJust (printTree x)

type Env = Map.Map String Ast