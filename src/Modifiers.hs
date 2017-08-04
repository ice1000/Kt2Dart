{-# LANGUAGE ApplicativeDo #-}

module Modifiers where

import Control.Applicative
import Control.Monad

import Parsers
import Annotations

modifierP :: Parser String
modifierP = classModifierP
  <|> accessModifierP
  <|> varianceAnnotationP
  <|> memberModifierP
  <|> parameterModifierP
  <|> typeParameterModifierP
  <|> functionModifierP
  <|> propertyModifierP
--

modifiersP :: Parser String
modifiersP = do
  ls <- option0 [] $ spacesP \|/ modifierP <|> annotationsP
  return $ join ls
--

typeModifiersP :: Parser String
typeModifiersP = do
  ls <- option0 [] $ spacesP \|/ suspendModifierP <|> annotationsP
  return $ join ls
--

classModifierP :: Parser String
classModifierP = reservedLP "abstract"
  <|> reservedLP "final"
  <|> reservedLP "enum"
  <|> "open" ->> []
  <|> reservedLP "annotation"
  <|> "sealed" ->> "abstract"
  <|> "data" ->> []
--

memberModifierP :: Parser String
memberModifierP = reservedLP "final"
  <|> reservedLP "abstract"
  <|> "open" ->> []
  <|> "lateinit" ->> []
  <|> "override" ->> "@override"
--

accessModifierP :: Parser String
accessModifierP = reservedLP "public"
  <|> reservedLP "private"
  <|> reservedLP "protected"
  <|> "internal" ->> []
--

varianceAnnotationP :: Parser String
varianceAnnotationP = reservedLP "in" <|> reservedLP "out"

parameterModifierP :: Parser String
parameterModifierP = flip convertParserP [] $ reservedLP "noinline"
  <|> reservedLP "crossinline"
  <|> reservedLP "vararg"
--

typeParameterModifierP :: Parser String
typeParameterModifierP = reservedP "reified"

functionModifierP :: Parser String
functionModifierP = "tailrec" ->> []
  <|> reservedP "operator"
  <|> "infix" ->> []
  <|> reservedP "inline"
  <|> "external" ->> "/* This is a JNI function */"
  <|> suspendModifierP
--

propertyModifierP :: Parser String
propertyModifierP = reservedP "const"

suspendModifierP :: Parser String
suspendModifierP = reservedP "suspend"
