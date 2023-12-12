module Types where

import qualified Data.Map as Map

data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show, Eq)

data Ast = Define SExpr SExpr Ast
         | AstInt Int
         | AstSymbol String
         | AstList [Ast]
         deriving (Show, Eq)

type Env = Map.Map String Ast