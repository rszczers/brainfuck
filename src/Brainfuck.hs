module Brainfuck where

import Parser

-- Abstract syntax tree for Brainfuck
data Symbol = Next              -- >
            | Prev              -- <
            | Inc               -- +
            | Dec               -- -
            | Print             -- .
            | Read              -- ,
            | Loop [Symbol]     -- [Symbol]
         deriving (Eq, Show)

type Program = [Symbol]

parseOne :: Parser Symbol
parseOne = choice [ transform '>' Next
                  , transform '<' Prev
                  , transform '+' Inc
                  , transform '-' Dec
                  , transform ',' Read
                  , transform '.' Print
                  , parseLoop
                  ]

parseLoop :: Parser Symbol
parseLoop = do
  consume '['
  steps <- many parseOne
  consume ']'
  return (Loop steps)

parseProgram :: Parser Program
parseProgram = do
  exprs <- many parseOne
  return exprs

parse :: String -> Program
parse = fst . head . runParser parseProgram . filter (`elem` "><+-,.[]")

