module Main where

import Brainfuck
import Evaluator
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= opt >>= (runProgram . parse)

opt ["-h"] = usage >> exitWith ExitSuccess
opt []     = getContents
opt fs     = concat <$> mapM readFile fs

usage = putStrLn "Usage: bf [-h] [file ..]"
