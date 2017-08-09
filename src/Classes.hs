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

-- | should deal with statics
classBodyP :: Parser [String]
classBodyP = do
  reservedLP "{"
  m <- membersP
  reservedLP "}"
  return m
--

-- | should deal with statics
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
    return $ join n
  b <- classBodyP
  -- TODO
  return $ "/* WARNING: companion object " ++ n
    ++ ":" ++ d ++ " is converted into static methods */"
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

objectP :: Parser String
objectP = do
  reservedLP "object"
  n <- simpleNameP
  c <- primaryConstructorP
  d <- option0 [] $ do
    reservedLP ":"
    n <- reservedLP "," \|/ delegationSpecifierP
    return $ join n
  b <- classBodyP
  -- TODO
  return $ "class " ++ n ++ join b
--  

-- | maybe I just need the parameters
primaryConstructorP :: Parser String
primaryConstructorP = do
  m <- option0 [] $ do
    m <- modifiersP
    reservedLP "constructor"
    return m
  b <- bracketsP $ reservedLP "," \|/ functionParameterP
  return $ join b
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

getterP :: Parser String
getterP = do
  m <- modifiersP
  reservedLP "get"
  (t, b) <- option0 ([], []) $ do
    bracketsP newLines0P
    t <- option0 [] $ do
      reservedLP ":"
      typeP
    b <- functionBodyP
    return (t, b)
  -- TODO
  undefined
--
