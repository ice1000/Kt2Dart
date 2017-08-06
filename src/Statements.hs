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
    (h : t) -> h : [ "else " ++ e | e <- t ]
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
      reservedLP "->"
      cs <- controlStructureBodyP
      semiP
      return $ "if(" ++ join wc ++ ")" ++ cs
    -- b = do
    --   reservedLP "else"
    --   reservedLP "->"
    --   cs <- controlStructureBodyP
    --   semiP
    --   return $ "default:" ++ cs ++ ";break;"
    b = do
      reservedLP "else"
      reservedLP "->"
      cs <- controlStructureBodyP
      semiP
      -- becase the `else` keyword will be added by `whenP`
      return cs
--

-- | s is expected to be not empty
whenConditionP :: String -> Parser String
whenConditionP s = in' <|> is' <|> expressionP
  where
    in' = do
      r <- reservedLP "in" <|> reservedLP "!in"
      e <- expressionP
      return $ if head r == '!'
        then '!' : e ++ ".contains(" ++ s ++ ")"
        else e ++ ".contains(" ++ s ++ ")"
    is' = do
      r <- reservedLP "is" <|> reservedLP "!is"
      t <- typeP
      return $ if head r == '!'
        then "!(" ++ s ++ " is " ++ t ++ ")"
        else s ++ " is " ++ t
--
