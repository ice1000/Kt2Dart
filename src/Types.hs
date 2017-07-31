module Types where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Rules

typeP :: Parser String
typeP = do
  m <- typeModifiersP
  r <- typeReferenceP
  return $ m ++ r
--

typeReferenceP :: Parser String
typeReferenceP = do
  a <- o
  reservedP "" <~> reservedP "?"
  return a
  where o = reservedP "dynamic"
          <|> functionTypeP
          <|> userTypeP
          -- <|> nullableTypeP
          <|> bracketsP typeReferenceP
--

-- nullableTypeP :: Parser String
-- nullableTypeP = do
--   t <- typeReferenceP
--   reservedP "?"
--   return t
-- --

typeArgumentsP :: Parser String
typeArgumentsP = do
  reservedP "<"
  ts <- reservedP "," \|/ typeP
  reservedP ">"
  return $ '<' : join ts ++ ">"
--

typeParametersP :: Parser String
typeParametersP = do
  reservedP "<"
  tps <- reservedP "," \|/ typeParameterP
  reservedP ">"
  return $ '<' : join tps ++ ">"
--

typeParameterP :: Parser String
typeParameterP = do
  m <- modifiersP
  n <- simpleNameP
  e <- option0 [] $ do
    reservedP ":"
    t <- userTypeP
    return $ "extends " ++ t
  return $ m ++ n ++ e
--

typeAliasP :: Parser String
typeAliasP = do
  m <- modifiersP
  reservedP "typealias"
  n <- simpleNameP
  spaces0P
  p <- option0 [] typeParametersP
  reservedP "="
  t <- typeP
  return $ m ++ " class " ++ n ++ p ++ " extends " ++ t
--

userTypeP :: Parser String
userTypeP = do
  ls <- reservedP "." \|/ simpleUserTypeP
  return $ join ls
--

optionalProjectionP :: Parser String
optionalProjectionP = varianceAnnotationP

simpleUserTypeP :: Parser String
simpleUserTypeP = do
  n <- simpleNameP
  spaces0P
  p <- reservedP [] <~> do
    reservedP "<"
    ls <- reservedP "," \|/ reservedP "*" <|> do
      o <- option0 [] optionalProjectionP
      typeP
    reservedP ">"
    return $ '<' : join ls ++ ">"
  return $ n ++ p
--

-- | Here's an issue
--   The `simple name` is `paramter` in the doc
functionTypeP :: Parser String
functionTypeP = do
  b <- bracketsP $ option0 [] $ reservedP "," \|/ simpleNameP
  reservedP "->"
  c <- typeP
  return "Function" -- "(" ++ join b ++ ")" ++ c
--

typeConstraintsP :: Parser String
typeConstraintsP = option0 [] $ do
  reservedP "where"
  ls <- reservedP "," \|/ typeConstraintP
  return $ join ls
--

typeConstraintP :: Parser String
typeConstraintP = do
  m <- modifiersP
  n <- simpleNameP
  u <- option0 [] userTypeP
  return $ m ++ n ++ u
--
