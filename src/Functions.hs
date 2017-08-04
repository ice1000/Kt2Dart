{-# LANGUAGE ApplicativeDo #-}

module Functions where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Statements
import {-# SOURCE #-} Types

functionP :: Parser String
functionP = do
  reservedP "fun"
  spaces0P
  tp <- option0 [] typeParametersP
  spaces0P
  rc <- option0 [] $ do
    t <- typeP
    reservedLP "."
    return t
  sn <- simpleNameP
  vp <- valueParametersP
  rt <- option0 [] $ do
    reservedLP ":"
    newLines0P
    t <- typeP
    return $ t ++ " "
  tc <- typeConstraintsP
  fb <- option0 [] functionBodyP
  return $ rt ++ sn ++ tp ++ vp ++ fb
--

functionBodyP :: Parser String
functionBodyP = blockP <~> do
  reservedLP "="
  e <- expressionP
  return $ "=>" ++ e
--

functionLiteralP :: Parser String
functionLiteralP = a <|> do
  reservedLP "{"
  newLines0P
  p <- reservedLP "," \|/ lambdaParameterP
  reservedLP "->"
  s <- statementsP
  reservedLP "}"
  return $ '(' : join p ++ "){" ++ s ++ "}"
  where
    a = do
      b <- blockP
      return $ "()" ++ b
--
