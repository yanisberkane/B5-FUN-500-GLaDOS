module Formatter (
    -- *Formatter
    -- $formatter
    formatEnv,
    formatInsts
) where

import VMTypes (Value(..), Instruction(..), VMEnv, Insts)

{- $formatter
    This module contains the formatter used in the Glados interpreter.
-}

formatEnv :: VMEnv -> String -- ^ Format an environment to a string
formatEnv env = unlines $ map formatBinding env
  where
    formatBinding (var, val) = var ++ " = " ++ showValue val
    showValue (IntValue i) = "INT " ++ show i
    showValue (BoolValue b) = "BOOL " ++ show b
    showValue (StringValue s) = "STRING " ++ s
    showValue (Operator op) = "OPERATOR " ++ show op
    showValue (Function insts) = "FUNCTION " ++ show insts
    showValue (ListValue vals) = "LIST " ++ show vals


formatInsts :: Insts -> String -- ^ Format a list of instructions to a string
formatInsts insts = unlines $ map show insts