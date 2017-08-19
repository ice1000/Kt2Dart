{-# LANGUAGE ApplicativeDo #-}

module Kotlin where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import Annotations
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Functions
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Types
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Classes

kotlinFileP :: Parser String
kotlinFileP = do
  p <- preambleP
  t <- many topLevelObjectP
  return $ p ++ join t
--

-- | This is probably useless.
scriptP :: Parser String
scriptP = do
  p <- preambleP
  e <- many expressionP
  return $ p ++ join e
--

preambleP :: Parser String
preambleP = do
  a <- option0 [] fileAnnotationsP
  h <- option0 [] packageHeaderP
  i <- many importP
  return $ join a ++ h ++ join i
--

packageHeaderP :: Parser String
packageHeaderP = do
  m <- modifiersP
  reservedLP "package"
  n <- simpleTokensP
  option0 ' ' semiP
  return $ "package " ++ join n
--

importP :: Parser String
importP = do
  reservedLP "import"
  n <- simpleTokensP
  t <- option0 [] $ do
    reservedLP "."
    reservedLP "*" <|> do
      reservedLP "as"
      tokenLP simpleNameP
  option0 ' ' semiP
  return $ "import '" ++ join n ++ "'"
--

fileAnnotationsP :: Parser [String]
fileAnnotationsP = many fileAnnotationP

fileAnnotationP :: Parser String
fileAnnotationP = do
  reservedLP "@"
  reservedLP "file"
  reservedLP ":"
  u <- unescapedAnnotationP <|> do
    reservedLP "["
    u <- some unescapedAnnotationP
    reservedLP "]"
    return $ join u
  return $ "/* WARNING: file annotation " ++ u ++ " is not supported */"
--

topLevelObjectP :: Parser String
topLevelObjectP = classP
  <|> objectP
  <|> functionP
  <|> propertyP
  <|> typeAliasP
--
