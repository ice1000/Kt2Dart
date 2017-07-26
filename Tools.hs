{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Tools where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

data OpTree a b =
  Op (OpTree a b) a (OpTree a b) 
  | Term b 
  deriving (Show, Eq, Functor)
--

data Associativity a =
  L a
  | R a
  | NoAssociativity a
  deriving (Show, Eq, Functor)
--

op :: String -> a -> ReadP a
op = op1 . string

op1 :: ReadP a -> b -> ReadP b
op1 s = (s >>) . return

oneOf :: String -> ReadP Char
oneOf ls = satisfy (`elem` ls)

spaces :: ReadP String
spaces = many $ oneOf " \n\t"

reserved :: ReadP a -> ReadP a
reserved r = do
  a <- r
  spaces
  return a
--

oneTerm :: ReadP b -> ReadP (OpTree a b)
oneTerm rp = do
  a <- reserved rp
  return $ Term a
--

makeChain :: ReadP a -> ReadP (OpTree a b -> OpTree a b -> OpTree a b)
makeChain p = do
  a <- reserved p
  return $ \x y -> Op x a y
--

brackets :: ReadP b -> ReadP b
brackets a = do
  reserved $ string "("
  r <- reserved a
  string ")"
  return r
--

chainm1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainm1 p op = do
  a <- p
  (do
      f <- op
      b <- p
      return $ f a b)
    <|> return a
--
