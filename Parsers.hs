{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative

alpha = ['a'..'z'] ++ ['A'..'Z']

-----------------------------------------------------
--------------- my parser combinator ----------------
-----------------------------------------------------

newtype Parser val = Parser { parse :: String -> [(val, String)]  }

parseCode :: Parser a -> String -> a
parseCode m s = case parse m s of
  [(res, [])] -> res
  _           -> error "Hugh?"
--

instance Functor Parser where
  fmap f (Parser ps) = Parser $ \p -> [ (f a, b) | (a, b) <- ps p ]
--

instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) = Parser $ \p ->
    [ (f a, s2) | (f, s1) <- p1 p, (a, s2) <- p2 s1 ]
--

instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  p >>= f  = Parser $ concatMap (\(a, s1) -> f a `parse` s1) . parse p
--

instance MonadPlus Parser where
  mzero     = Parser $ const []
  mplus p q = Parser $ \s -> parse p s ++ parse q s
--

instance Alternative Parser where
  empty   = mzero
  p <|> q = Parser $ \s -> case parse p s of
    [] -> parse q s
    rs -> rs
--

item :: Parser Char
item = Parser $ \s -> case s of
  [     ] -> [      ]
  (h : t) -> [(h, t)]
--

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else empty

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a = (do
      f <- op
      b <- p
      rest $ f a b)
      <|> return a
--

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = do
      a <- p
      rest a
    rest a = (do
      f <- op
      b <- scan
      rest $ f a b)
      <|> return a
--

chainm1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainm1 p op = do
  a <- p
  (do
      f <- op
      b <- p
      return $ f a b)
    <|> return a
--

chainl p op = (chainl1 p op <|>) . return

op :: String -> a -> Parser a
op = op1 . string

op1 :: Parser a -> b -> Parser b
op1 s = (s >>) . return

brackets :: Parser b -> Parser b
brackets m = do
  reserved "("
  n <- m
  reserved ")"
  return n
--

oneOf :: String -> Parser Char
oneOf = satisfy . flip elem

char = satisfy . (==)

nat :: Parser Int
nat = read <$> some digit

digit = satisfy isDigit

reserved = token . string

spaces :: Parser String
spaces = many $ oneOf " \n\r\t"

string [      ] = return []
string (c : cs) = do
  char c
  string cs
  return $ c : cs
--

token p = do
  a <- p
  spaces
  return a
--

numberP :: Parser Int
numberP = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read $ s ++ cs
--
