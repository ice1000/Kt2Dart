{-# LANGUAGE ApplicativeDo #-}

module Classes where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Functions
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Types
import {-# SOURCE #-} Modifiers

anonymousInitializerP :: Parser String
anonymousInitializerP = do
  reservedLP "init"
  blockP
--

classBodyP :: Parser [String]
classBodyP = do
  reservedLP "{"
  m <- membersP
  reservedLP "}"
  return m
--

membersP :: Parser [String]
membersP = many memberDelarationP

companionObjectP :: Parser String
companionObjectP = do
  m <- modifiersP
  reservedLP "companion"
  reservedLP "object"
  n <- option0 "Companion" simpleNameP
  d <- option0 [] $ do
    reservedLP ":"
    n <- reservedLP "," \|/ delegationSpecifierP
    return $ take 1 n
  b <- classBodyP
  return $ "/* WARNING: companion object " ++ n ++ " is converted into static methods */"
--

memberDelarationP :: Parser String
memberDelarationP = typeAliasP
  <|> companionObjectP
  <|> functionP
  <|> anonymousInitializerP
--  <|> objectP
--  <|> classP
--  <|> propertyP
--  <|> secondaryConstructerP
--

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

