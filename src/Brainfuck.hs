module Brainfuck where

import Parser

-- | Parser can be viewed directly as a function that takes a string
-- and produces a abstract syntax tree (AST). Symbol datatype is an AST
-- to represent brainfuck instruction set.
data Symbol = Next              -- ^ > symbol; increments the data pointer
            | Prev              -- ^ < symbol; decrements the data pointer
            | Inc               -- ^ + symbol; increments the byte at the data pointer
            | Dec               -- ^ - symbol; decrements the byte at the data pointer
            | Print             -- ^ . symbol; output the byte at the data pointer
            | Read              -- ^ , symbol; get input and store it in the byte at the data pointer
            | Loop [Symbol]     -- ^ [Symbol]; loop all instructions between pair of [] until byte at the data pointer reaches 0
         deriving (Eq, Show)

-- | Brainfuck program is a list of commands represented by Symbols.
type Program = [Symbol]

-- | Parses specified characters to Symbols by trying to apply parsers
-- in given order
parseOne :: Parser Symbol
parseOne = choice [ transform '>' Next
                  , transform '<' Prev
                  , transform '+' Inc
                  , transform '-' Dec
                  , transform ',' Read
                  , transform '.' Print
                  , parseLoop
                  ]

-- | Parses bf's loops instructions by matching and ommiting a square
-- brackets. Then it performs parsing in loop body and wraps results
-- in the Loop Symbol.
parseLoop :: Parser Symbol
parseLoop = do
  consume '['
  steps <- many parseOne
  consume ']'
  return (Loop steps)

-- Repeatedly applies parseOne parser on input and yelds AST
parseProgram :: Parser Program
parseProgram = do
  exprs <- many parseOne
  return exprs

-- | Parses given String to internal representation of brainfuck
-- program.
parse :: String -> Program
parse = fst . head .          -- returns best match (first element of head of result)
  runParser parseProgram .    -- runs parser
  filter (`elem` "><+-,.[]")  -- assures us that only legal brainfuck symbols are going to be parsed and rest ignored.

