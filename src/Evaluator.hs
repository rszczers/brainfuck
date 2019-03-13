module Evaluator where

import Brainfuck
import Data.Char (chr, ord)
import System.IO (hFlush, stdout)
import Data.List (intercalate)

data Tape a = Tape [a] a [a]

instance Show a => Show (Tape a) where
    show (Tape l c r) = "..." ++ (show' . reverse .  take 3 $ l) ++ " >" ++  show c ++ "< " ++ show' (take 3 r) ++ "..."
      where
        show' xs = intercalate " " $ show <$> xs

instance Functor Tape where
  fmap f (Tape l a r) = Tape (f <$> l) (f a) (f <$> r)

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
  where zeros = repeat 0

nextSymbol :: Maybe (Tape a) -> Maybe (Tape a)
nextSymbol (Just (Tape ls p (r:rs))) = Just $ Tape (p:ls) r rs
nextSymbol _ = Nothing

prevSymbol :: Maybe (Tape a) -> Maybe (Tape a)
prevSymbol (Just (Tape (l:ls) p rs)) = Just $ Tape ls l (p:rs)
prevSymbol _ = Nothing

nextCell :: Tape a -> Tape a
nextCell (Tape ls p (r:rs)) = Tape (p:ls) r rs

prevCell :: Tape a -> Tape a
prevCell (Tape (l:ls) p rs) = Tape ls l (p:rs)

eval :: (Tape Int) -> Maybe (Tape Symbol) -> IO (Tape Int)
eval reg prog@(Just (Tape _ Next _)) =
  eval (nextCell reg) (nextSymbol prog)
eval reg prog@(Just (Tape _ Prev _)) =
  eval (prevCell reg) (nextSymbol prog)
eval (Tape l c r) prog@(Just (Tape _ Inc _)) =
  eval (Tape l (c + 1) r) (nextSymbol prog)
eval (Tape l c r) prog@(Just (Tape _ Dec _)) =
  eval (Tape l (c - 1) r) (nextSymbol prog)
eval reg@(Tape _ c _) prog@(Just (Tape _ Print  _)) =
  putChar (chr c) >>
  hFlush stdout >>
  eval reg (nextSymbol prog)
eval reg@(Tape l _ r) prog@(Just (Tape _ Read _)) =
  getChar >>= \c ->
  eval (Tape l (ord c) r) (nextSymbol prog)
eval reg@(Tape _ c _) prog@(Just (Tape _ (Loop s) r)) = case (c > 0) of
  True  -> do
    tape <- eval reg (Just (Tape [] (head s) (tail s)))
--    putStrLn . show $ reg
    eval tape prog
  False -> eval reg (nextSymbol prog)
eval reg Nothing = return reg

runProgram :: Program -> IO ()
runProgram [] = return ()
runProgram (x:xs) = do
    tape <- eval emptyTape (Just (Tape [] x xs))
    --putStrLn . show $ tape
    return ()
