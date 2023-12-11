module Main where

import qualified InterpreterTest

main :: IO ()
main = do
    putStrLn "--Running Tests--"
    InterpreterTest.runTests
    putStrLn "--Tests Completed--"