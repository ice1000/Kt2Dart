{-# LANGUAGE ApplicativeDo #-}

module Kotlin where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Functions
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Types
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Classes

kotlinFileP :: Parser String
kotlinFileP = do
  p <- preamble
  t <- many topLevelObjectP
  return $ p ++ join t
--

scriptP :: Parser String
scriptP = do
  p <- preamble
  e <- many expressionP
  return $ p ++ join e
--

topLevelObjectP :: Parser String
topLevelObjectP = classP
  <|> objectP
  <|> functionP
  <|> propertyP
  <|> typeAliasP
--
