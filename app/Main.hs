module Main where

import Brainfuck
import Evaluator
import System.Environment
import System.Exit

-- | Brainfuck interpreter

main :: IO ()
main = getArgs >>=  -- get inline arguments
    opt >>=         -- print help or pass contents of source file to run
    (runProgram . parse)

opt ["-h"] = usage >> exitWith ExitSuccess -- print help and exit
opt []     = getContents -- allows to pass contents of file
                         -- in command line, like: bf < cat source.bf
opt fs     = concat <$> mapM readFile fs -- read multiple files 
                                         -- (sources are concatenated
                                         -- and evaluated as one 
                                         -- program)
-- | Prints help
usage = putStrLn "Usage: bf [-h] [file ..]"
