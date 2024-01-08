module Types where

import qualified Data.Map as Map
import Data.Maybe

data Ast = AstInt Int
         | AstBool Bool
         | AstSymbol String
         | AstList [Ast]
         | Define Ast Ast
         | AstString String
         | Lambda Ast Ast
         | NamedFunc Ast Ast
         | Separator Char
         | If Ast Ast Ast
         deriving (Show, Eq)

type Env = Map.Map String Ast