{-# LANGUAGE ApplicativeDo #-}

module Classes where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Types

anonymousInitializerP :: Parser String
anonymousInitializerP = do
  reservedLP "init"
  blockP
--

classBodyP :: Parser String
classBodyP = do
  reservedLP "{"
  m <- membersP
  reservedLP "}"
  return m
--

membersP :: Parser String
membersP = do
  m <- many memberDelarationP
  return $ join m
--

memberDelarationP :: Parser String
memberDelarationP = undefined

delegationSpecifierP :: Parser String
delegationSpecifierP = constructorInvocationP
  <|> explicitDelegationP
  <|> userTypeP
--

explicitDelegationP :: Parser String
explicitDelegationP = do
  t <- userTypeP
  reservedLP "by"
  e <- expressionP
  return $ "/* WARNING: delegation " ++ e ++ " to " ++ t ++ " is not supported */"
--

