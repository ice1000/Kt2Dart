{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module OperatorP where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

import Tools

parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators arr rp = foldr fu tm arr
  where tm = oneTerm rp <|> brackets re
        re = foldr fu tm arr
        fu :: Associativity [ReadP a] -> ReadP (OpTree a b) -> ReadP (OpTree a b)
        fu (L               a) o = chainl1 o $ foldr1 (<|>) $ makeChain <$> a
        fu (R               a) o = chainr1 o $ foldr1 (<|>) $ makeChain <$> a
        fu (NoAssociativity a) o = chainm1 o $ foldr1 (<|>) $ makeChain <$> a
--
