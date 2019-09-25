module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    putStrLn "Type in a lisp expression:"
    name <- getLine
    putStrLn (readExpression name)