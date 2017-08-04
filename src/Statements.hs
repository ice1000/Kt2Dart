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
      return $ "return " ++ l ++ e
    continueP = jumper "continue"
    breakP = jumper "break"
--
