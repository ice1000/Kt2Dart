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

flattenTree :: (String -> String) -> (b -> String) -> OpTree String b -> String
flattenTree _  fb (Term       t) = fb t
flattenTree fa fb (Op x "in"  y) = flattenTree fa fb y ++ ".contains(" ++ flattenTree fa fb x ++ ")"
flattenTree fa fb (Op x "!in" y) = "!" ++ flattenTree fa fb y ++ ".contains(" ++ flattenTree fa fb x ++ ")"
flattenTree fa fb (Op x "is"  y) = "(" ++ flattenTree fa fb x ++ " is " ++ flattenTree fa fb y ++ ")"
flattenTree fa fb (Op x "!is" y) = "!(" ++ flattenTree fa fb x ++ " is " ++ flattenTree fa fb y ++ ")"
flattenTree fa fb (Op x  o    y) = if head o == '#'
  then flattenTree fa fb x ++ "." ++ fa (tail o) ++ "(" ++ flattenTree fa fb y ++ ")"
  else if o `elem` [ ".", "?.", "," ] then flattenTree fa fb x ++ fa o ++ flattenTree fa fb y
  else "(" ++ flattenTree fa fb x ++ fa o ++ flattenTree fa fb y ++ ")"
--

reserved'' :: Parser b -> Parser b
reserved'' b = do
  a <- b
  spaces0P
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
