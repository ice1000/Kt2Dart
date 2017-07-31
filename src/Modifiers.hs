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
  ls <- spaces0P \|/ modifierP <|> annotationsP
  return $ join ls
--

typeModifiersP :: Parser String
typeModifiersP = do
  ls <- spaces0P \|/ suspendModifierP <|> annotationsP
  return $ join ls
--

classModifierP :: Parser String
classModifierP = reservedP "abstract"
  <|> reservedP "final"
  <|> reservedP "enum"
  <|> "open" =>> []
  <|> reservedP "annotation"
  <|> "sealed" =>> "abstract"
  <|> "data" =>> []
--

memberModifierP :: Parser String
memberModifierP = reservedP "final"
  <|> reservedP "abstract"
  <|> "open" =>> []
  <|> "lateinit" =>> []
  <|> "override" =>> "@override"
--

accessModifierP :: Parser String
accessModifierP = reservedP "public"
  <|> reservedP "private"
  <|> reservedP "protected"
  <|> "internal" =>> []
--

varianceAnnotationP :: Parser String
varianceAnnotationP = reservedP "in" <|> reservedP "out"

parameterModifierP :: Parser String
parameterModifierP = flip convertParserP [] $ reservedP "noinline"
  <|> reservedP "crossinline"
  <|> reservedP "vararg"
--

typeParameterModifierP :: Parser String
typeParameterModifierP = reservedP "reified"

functionModifierP :: Parser String
functionModifierP = "tailrec" =>> []
  <|> reservedP "operator"
  <|> "infix" =>> []
  <|> reservedP "inline"
  <|> "external" =>> "/* This is a JNI function */"
  <|> suspendModifierP
--

propertyModifierP :: Parser String
propertyModifierP = reservedP "const"

suspendModifierP :: Parser String
suspendModifierP = reservedP "suspend"
