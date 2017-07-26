{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module OperatorP where

import Parsers
import Control.Applicative

data OpTree a b = Op (OpTree a b) a (OpTree a b) 
                | Term b 
  deriving (Show, Eq, Functor)
--

data Assoc a = La a
             | Ra a
             | Na a
  deriving (Show, Eq, Functor)
--

reserved'' :: Parser b -> Parser b
reserved'' b = do
  a <- b
  spacesP
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

parseOperators :: [Assoc [Parser a]] -> Parser b -> Parser (OpTree a b)
parseOperators arr rp = foldr fu tm arr
  where tm = oneTerm rp <|> bracketsP re
        re = foldr fu tm arr
        fu :: Assoc [Parser a] -> Parser (OpTree a b) -> Parser (OpTree a b)
        fu (La a) o = chainl1 o $ foldr1 (<|>) $ makeChain <$> a
        fu (Ra a) o = chainr1 o $ foldr1 (<|>) $ makeChain <$> a
        fu (Na a) o = option1 o $ foldr1 (<|>) $ makeChain <$> a
--
