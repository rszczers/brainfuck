module Parser where

import Control.Applicative hiding (some, many)
import Control.Applicative        (Alternative)

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

instance Alternative Parser where
  empty = Parser $ \inp -> []
  p <|> q = Parser $ 
    \inp -> case (runParser p inp) of
              []       -> runParser q inp
              [(v, o)] -> [(v, o)]

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \inp -> concat
    [runParser (f v) inp'
    | (v, inp') <- (runParser p) inp]

item :: Parser Char
item = Parser $ \inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= 
  \x -> if p x then return x else empty

char :: Char -> Parser Char
char x = satisfy (\y -> x == y)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

consume :: Char -> Parser ()
consume c = satisfy (== c) >> return ()

many :: Alternative f => f a -> f [a]
many x = some x <|> pure []

some :: Alternative f => f a -> f [a]
some x = pure (:) <*> x <*> many x

transform :: Char -> a -> Parser a
transform c s = char c >> return s
