{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Parsers where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative

-----------------------------------------------------
--------------- my parser combinator ----------------
-----------------------------------------------------

newtype Parser val = Parser { parse :: String -> [(val, String)]  }

parseCode :: Parser a -> String -> Either String a
parseCode m s = case parse m s of
  [(res, [])] -> Right res
  _           -> Left "Hugh?"
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

(<~>) :: Alternative a => a b -> a b -> a b
(<~>) = flip (<|>)

item :: Parser Char
item = Parser $ \s -> case s of
  [     ] -> [      ]
  (h : t) -> [(h, t)]
--

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else empty

disatisfy :: (Char -> Bool) -> Parser Char
disatisfy p = satisfy $ not . p

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a = return a <~> do
      f <- op
      b <- p
      rest $ f a b
--

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = do
      a <- p
      rest a
    rest a = return a <~> do
      f <- op
      b <- scan
      rest $ f a b
--

option1 :: Parser a -> Parser (a -> a -> a) -> Parser a
option1 p op = do
  a <- p
  return a <~> do
      f <- op
      b <- p
      return $ f a b
--

option0 :: b -> Parser b -> Parser b
option0 d p = p <|> return d

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op = (chainl1 p op <|>) . return

convertStringP :: String -> a -> Parser a
convertStringP = convertParserP . stringP

convertParserP :: Parser a -> b -> Parser b
convertParserP s = (s >>) . return

bracketsP :: Parser b -> Parser b
bracketsP m = do
  reservedP "("
  n <- m
  reservedP ")"
  return n
--

oneOf :: String -> Parser Char
oneOf = satisfy . flip elem

noneOf :: String -> Parser Char
noneOf = disatisfy . flip elem

charP :: Char -> Parser Char
charP = satisfy . (==)

oneCharP :: Parser Char
oneCharP = satisfy $ const True

oneCharPS :: Parser String
oneCharPS = do
  c <- oneCharP
  return [c]
--

exceptCharP :: Char -> Parser Char
exceptCharP = disatisfy . (==)

reservedP :: String -> Parser String
reservedP = tokenP . stringP

reservedLP :: String -> Parser String
reservedLP = tokenLP . stringP

convertReservedP :: String -> String -> Parser String
convertReservedP a = tokenP . convertStringP a

convertReservedLP :: String -> String -> Parser String
convertReservedLP a = tokenLP . convertStringP a

spacesP :: Parser String
spacesP = do
  some $ oneOf " \r\t"
  return " "
--

spaces0P :: Parser String
spaces0P = option0 [] spacesP

newLinesP :: Parser String
newLinesP = do
  some $ oneOf " \t\r\n"
  return " "
--

newLines0P :: Parser String
newLines0P = option0 [] newLinesP

stringP :: String -> Parser String
stringP [      ] = return []
stringP (c : cs) = do
  charP c
  stringP cs
  return $ c : cs
--

tokenP :: Parser String -> Parser String
tokenP p = do
  a <- p
  s <- spaces0P
  return $ a ++ s
--

tokenLP :: Parser String -> Parser String
tokenLP p = do
  a <- p
  s <- newLines0P
  return $ a ++ s
--

seperateP :: Parser String -> Parser String -> Parser [String]
seperateP ns ss = do
  n <- ns
  return [n] <~> do
    s <- ss
    r <- seperateP ns ss
    return $ n : s : r
--

(\|/) = flip seperateP
(=>>) = convertReservedP
(->>) = convertReservedLP

digitP :: Parser Char
digitP = satisfy isDigit

infixl 2 \|/
