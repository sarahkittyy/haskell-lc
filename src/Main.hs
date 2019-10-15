module Main where

import System.Environment
import Data.List

import Parser.Grammar
import Parser.Parser

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show $ runParser parseExpr $ intercalate " " args