{-
-- EPITECH PROJECT, 2024
-- B5-FUN-500-GLaDOS-Mirror [WSL: Ubuntu]
-- File description:
-- Types
-}
module Types (
    -- *Types
    -- $type
    Ast(..),
) where

import qualified Data.Map as Map
import Data.Maybe

{- $type
    This module contains the types used in the Glados interpreter.

    Ast is the type used to represent the abstract syntax tree.

-}

data Ast =
           AstInt Int -- ^ AstInt represents an integer both positive and negative. (0, 1, 2, -1, -2)
         | AstBool Bool -- ^ AstBool represents a boolean. (True or False)
         | AstSymbol String -- ^ AstSymbol represents a symbol consisting of Alpha characters. (a-z, A-Z)
         | AstString String -- ^ AstString represents a string ("Example string").
         | AstList [Ast] -- ^ AstList represents a list of Asts. ((1 hello 3))
         | AstMathOp Ast Ast Ast -- ^ AstMathOp represents a mathematical operation. (x + y, x - y, x * y, x / y, x % y)
         | Assign Ast Ast -- ^ AstLogicOp represents a logical operation. (x = y)
         | Define Ast Ast -- ^ Define represents a variable declaration. (let, var)
         | If Ast Ast Ast -- ^ If represents an if statement (also works as a ternary). (if)
         | Lambda Ast Ast -- ^ Lambda represents a lambda function. (lambda)
         | NamedCall Ast Ast -- ^ NamedCall represents a named function. (namedCall)
         | AstCall Ast Ast -- ^ Call represents a function call. (call)
         | Separator Char -- ^ Separator represents a separator character. (;)
         | AstOperator String -- ^ AstOperator represents an operator. (+, -, *, /)
         | LogicOperator String -- ^ LogicOperator represents a logical operator. (&&, ||, !, ==, !=, <, >, <=, >=)
         | AstNone -- ^ AstNone represents nothing.
         deriving (Show, Eq)