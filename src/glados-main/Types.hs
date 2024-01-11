module Types where

import qualified Data.Map as Map
import Data.Maybe

data Ast = AstInt Int
         | AstBool Bool
         | AstSymbol String
         | AstString String
         | AstList [Ast]
         | AstMathOp Ast Ast Ast
         | Assign Ast Ast
         | Define Ast Ast
         | If Ast Ast Ast
         | Lambda Ast Ast
         | NamedCall Ast Ast
         | AstCall Ast Ast
         | Separator Char
         | AstOperator String
         | LogicOperator String
         | AstNone
         deriving (Show, Eq)

type Env = Map.Map String Ast