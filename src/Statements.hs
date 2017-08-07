{-# LANGUAGE ApplicativeDo #-}

module Statements where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import Annotations
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Types
import {-# SOURCE #-} Expressions

statementsP :: Parser String
statementsP = do
  many semiP
  s <- option0 [] $ some semiP \|/ statementP
  many semiP
  return $ join s ++ if s /= [] then ";" else []
--

statementP :: Parser String
statementP = reservedLP "stmt"

jumpP :: Parser String
jumpP = throwP <|> returnP <|> continueP <|> breakP
  where
    throwP = do
      reservedLP "throw"
      e <- expressionP
      return $ "throw " ++ e
    jumper s = do
      stringP s
      l <- labelReferenceP <|> spaces0P
      return $ s ++ l
    returnP = do
      l <- jumper "return"
      e <- option0 [] expressionP
      return $ l ++ e
    continueP = jumper "continue"
    breakP = jumper "break"
--

-- | very complex
whenP :: Parser String
whenP = do
  reservedLP "when"
  c <- option0 [] $ bracketsP expressionP
  reservedLP "{"
  w <- many $ whenEntryP c
  reservedLP "}"
  return . join $ case w of
    [     ] -> []
    (h : t) -> h : [ "else" ++ g e | e <- t ]
  where g e@('i' : _) = ' ' : e
        g e           = e
--

-- | s is expected to be not empty
whenEntryP :: String -> Parser String
whenEntryP s = a <|> b
  where
    -- a = do
    --   wc <- "," ->> ":case " \|/ whenConditionP s
    --   reservedLP "->"
    --   cs <- controlStructureBodyP
    --   semiP
    --   return $ "case " ++ join wc ++ ":" ++ cs ++ ";break;"
    a = do
      wc <- "," ->> "||" \|/ whenConditionP s
      cs <- getCS
      return $ "if(" ++ join wc ++ ")" ++ cs

    -- b = do
    --   reservedLP "else"
    --   reservedLP "->"
    --   cs <- controlStructureBodyP
    --   semiP
    --   return $ "default:" ++ cs ++ ";break;"
    b = do
      reservedLP "else"
      cs <- getCS
      -- becase the `else` keyword will be added by `whenP`
      return cs
    getCS = do
      reservedLP "->"
      cs <- controlStructureBodyP
      semiP
      return cs
--

-- | s is expected to be not empty
whenConditionP :: String -> Parser String
whenConditionP s = in' <|> is' <|> ex'
  where
    in' = do
      r <- inOperationP
      e <- expressionP
      return $ if head r == '!'
        then '!' : e ++ ".contains(" ++ s ++ ")"
        else e ++ ".contains(" ++ s ++ ")"
    is' = do
      r <- isOperationP
      t <- typeP
      return $ if head r == '!'
        then "!(" ++ s ++ " is " ++ t ++ ")"
        else s ++ " is " ++ t
    ex' = do
      e <- expressionP
      return $ e ++ "==" ++ s
--

tryP :: Parser String
tryP = do
  reservedLP "try"
  t <- blockP
  c <- many catchBlockP
  f <- option0 [] finallyBlockP
  return $ t ++ join c ++ f
--

catchBlockP :: Parser String
catchBlockP = do
  reservedLP "catch"
  reservedLP "("
  a <- annotationsP
  n <- simpleNameP
  reservedLP ":"
  t <- userTypeP
  reservedLP ")"
  b <- blockP
  return $ "catch(" ++ optionalPrefix a ++ t ++ " " ++ n ++ "){" ++ b ++ "}"
--

finallyBlockP :: Parser String
finallyBlockP = do
  reservedLP "finally"
  b <- blockP
  return $ "finally{" ++ b ++ "}"
--

whileP :: Parser String
whileP = do
  reservedLP "while"
  reservedLP "("
  e <- expressionP
  reservedLP ")"
  b <- controlStructureBodyP
  return $ "while(" ++ e ++ ")" ++ b
--
