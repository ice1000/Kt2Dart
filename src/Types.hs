{-# LANGUAGE ApplicativeDo #-}

module Types where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Rules

typeP :: Parser String
typeP = do
  newLines0P
  m <- typeModifiersP
  r <- typeReferenceP
  return $ m ++ r
--

typeReferenceP :: Parser String
typeReferenceP = tokenLP $ do
  a <- o
  r <- option0 [] $ reservedLP "?"
  return a
  where o = reservedLP "dynamic"
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
  reservedLP "<"
  ts <- reservedLP "," \|/ typeP
  reservedLP ">"
  return $ '<' : join ts ++ ">"
--

typeParametersP :: Parser String
typeParametersP = do
  reservedLP "<"
  tps <- reservedLP "," \|/ typeParameterP
  reservedLP ">"
  return $ '<' : join tps ++ ">"
--

typeParameterP :: Parser String
typeParameterP = do
  newLines0P
  m <- modifiersP
  newLines0P
  n <- simpleNameP
  e <- option0 [] $ do
    reservedLP ":"
    t <- userTypeP
    return $ "extends " ++ t
  return $ m ++ n ++ e
--

typeAliasP :: Parser String
typeAliasP = do
  m <- modifiersP
  reservedLP "typealias"
  newLines0P
  n <- simpleNameP
  p <- option0 [] typeParametersP
  reservedLP "="
  t <- typeP
  return $ m ++ " class "
    ++ n ++ p ++ " extends " ++ t
--

userTypeP :: Parser String
userTypeP = do
  ls <- reservedLP "." \|/ simpleUserTypeP
  return $ join ls
--

optionalProjectionP :: Parser String
optionalProjectionP = varianceAnnotationP

-- | Maybe transform `*` into something else in dart
simpleUserTypeP :: Parser String
simpleUserTypeP = tokenLP $ do
  n <- simpleNameP
  p <- reservedP [] <~> do
    reservedLP "<"
    ls <- reservedLP "," \|/ reservedLP "*" <|> do
      o <- option0 [] optionalProjectionP
      typeP
    reservedLP ">"
    return $ '<' : join ls ++ ">"
  return $ typeNamesMapping n ++ p
--

-- | Here's an issue
--   The `simple name` is `paramter` in the doc
functionTypeP :: Parser String
functionTypeP = do
  b <- bracketsP $ option0 [] $
    reservedLP "," \|/ tokenLP typeP -- simpleNameP
  reservedLP "->"
  c <- typeP
  return $ "Function" ++ "/* ("
    ++ join (typeNamesMapping <$> b)
    ++ ") -> " ++ c ++ " */"
--

typeConstraintsP :: Parser String
typeConstraintsP = option0 [] $ do
  reservedP "where"
  l <- reservedP "," \|/ typeConstraintP
  return $ case l of
    [] -> []
    ls -> "/* type constraints "
      ++ join ls ++ " is not supported */"
--

typeConstraintP :: Parser String
typeConstraintP = do
  m <- modifiersP
  n <- simpleNameP
  u <- option0 [] userTypeP
  return $ m ++ n ++ u
--

typeNamesMapping :: String -> String
typeNamesMapping = mapped
  where
    mapped "Unit"    = "void"
    mapped "Int"     = "int"
    mapped "Boolean" = "bool"
    mapped "Double"  = "double"
    mapped "Float"   = "float"
    mapped "Long"    = "long"
    mapped "Array"   = "List"
    mapped others    = others
--
