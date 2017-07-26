{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module OperatorP where

import Parsers
import Control.Applicative

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

reserved'' :: Parser b -> Parser b
reserved'' b = do
  a <- b
  spaces
  return a
--

oneTerm :: Parser b -> Parser (OpTree a b)
oneTerm rp = do
  a <- reserved'' rp
  return $ Term a
--

makeChain :: Parser a -> Parser (OpTree a b -> OpTree a b -> OpTree a b)
makeChain p = do
  a <- reserved'' p
  return $ \x y -> Op x a y
--

parseOperators :: [Associativity [Parser a]] -> Parser b -> Parser (OpTree a b)
parseOperators arr rp = foldr fu tm arr
  where tm = oneTerm rp <|> brackets re
        re = foldr fu tm arr
        fu :: Associativity [Parser a] -> Parser (OpTree a b) -> Parser (OpTree a b)
        fu (L               a) o = chainl1 o $ foldr1 (<|>) $ makeChain <$> a
        fu (R               a) o = chainr1 o $ foldr1 (<|>) $ makeChain <$> a
        fu (NoAssociativity a) o = chainm1 o $ foldr1 (<|>) $ makeChain <$> a
--
