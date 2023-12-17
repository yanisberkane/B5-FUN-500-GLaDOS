module Types where

import qualified Data.Map as Map
import Data.Maybe

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           | SBool Bool
           | SString String
           deriving (Show, Eq)

data Ast = AstInt Int
         | AstBool Bool
         | AstSymbol String
         | AstList [Ast]
         | Define Ast Ast Ast
         | AstString String
         | Lambda [String] Ast
         | If Ast Ast Ast
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
sexprToAst (SList [SSymbol "if", cond, thenExpr, elseExpr]) =
    If <$> sexprToAst cond <*> sexprToAst thenExpr <*> sexprToAst elseExpr
sexprToAst (SSymbol str) = Just $ AstSymbol str
sexprToAst (SString str) = Just $ AstString str
sexprToAst (SInt int) = Just $ AstInt int
sexprToAst (SBool b) = Just $ AstBool b
sexprToAst (SList list) = AstList <$> mapM sexprToAst list
sexprToAst _ = Nothing

astToSExpr :: Ast -> SExpr
astToSExpr (If cond thenExpr elseExpr) = SList [SSymbol "if", astToSExpr cond, astToSExpr thenExpr, astToSExpr elseExpr]
astToSExpr (AstSymbol str) = SSymbol str
astToSExpr (AstInt int) = SInt int
astToSExpr (AstBool b) = SBool b
astToSExpr (AstList list) = SList $ map astToSExpr list

type Env = Map.Map String Ast