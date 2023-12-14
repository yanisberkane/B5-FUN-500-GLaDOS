module Types where

import qualified Data.Map as Map
import Data.Maybe

data SExpr = SInt Int
           | SSymbol String
           | SBool Bool
           | SList [SExpr]
           deriving (Show, Eq)

data Ast = AstInt Int
         | AstBool Bool
         | AstSymbol String
         | AstList [Ast]
         | Define Ast Ast Ast
         | Lambda [String] Ast
         | FunctionCall Ast [Ast]
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

type Env = Map.Map String Ast