module Parser where

-- | Parser module inspired by Graham Hutton's and Erik Meijer's
-- 'Monadic Parser Combinators' paper.

import Control.Applicative hiding (some, many)
import Control.Applicative        (Alternative)

-- | Parser is essentially a wraped-up function that takes a string
-- of characters as input and yields abstract syntax tree as result.
-- Moreover, parser might not consume all of its input string,
-- so rather than the result of a parser being just a tree, we also
-- return the unconsumed suffix of the input string. If parser fails,
-- it yelds empty list as result.
data Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = Parser $
    \inp -> case (runParser p inp) of
              []         -> []
              [(v, out)] -> [(f v, out)]

instance Applicative Parser where
  pure v  = Parser $ \inp -> [(v, inp)]
  pf <*> pa = Parser $
    \inp -> case (runParser pf inp) of
      [] -> []
      [(g, o)] -> runParser (g <$> pa) o

-- | Alternative instance of Parser. Enables us to use (<|>) operator
-- that will try the second alternative parser if the first parser
-- has failed and not consummed any input.
instance Alternative Parser where
  empty = Parser $ \inp -> []
  p <|> q = Parser $
    \inp -> case (runParser p inp) of
              []       -> runParser q inp
              [(v, o)] -> [(v, o)]

-- | Monad instance of Parser. Enables us to chain-up parsers and
-- use conviniet do-notation.
instance Monad Parser where
  return = pure
  p >>= f = Parser $ \inp -> concat
    [runParser (f v) inp'
    | (v, inp') <- (runParser p) inp]

-- | Parser which successfully consumes the first character if the
-- input string is non-empty, and fails otherwise
item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)]

-- | Combinator that takes a predicate (a Boolean valued function)
-- and yields a parser that consumes a single character if it
-- satisfies the predicate, and fails otherwise
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>=
  \x -> if p x then return x else empty

-- | Parser for specific characters
-- Example: runParser (char 'c') "ccabc" -> [("c", "cabc")]
char :: Char -> Parser Char
char x = satisfy (\y -> x == y)

-- | Ommits a specific character or fails if otherwise
-- Example: runParser (consume '{') "{foo}" -> [((), "foo}")]
consume :: Char -> Parser ()
consume c = satisfy (== c) >> return ()

-- | Tries to apply the parsers given in the list in order, until one
-- of them succeeds. Returns the value of the succeeding parser.
-- Example: src/Brainfuck.hs, parseOne definition
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | Takes a single function argument and repeatedly applies it until
-- the function fails and then yields the collected results up
-- to that point.
-- Example: runParser (many (char 'c')) "ccca" -> [("ccc", "a")]
many :: Alternative f => f a -> f [a]
many x = some x <|> pure []

-- | Takes a single function argument and fails if there's not at least
-- one match. Otherwise repeatedly applies given function and yelds
-- the collected results up to the point when it fails.
-- Example: runParser (some (char 'c')) "acccc" -> []
some :: Alternative f => f a -> f [a]
some x = pure (:) <*> x <*> many x

-- | Consumes character and if it matches specified character, it
-- parses it to specified symbol
-- Example: src/Brainfuck.hs, parserOne definition
transform :: Char -> a -> Parser a
transform c s = char c >> return s
