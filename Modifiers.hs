module Modifiers where

import Control.Applicative
import Control.Monad

import Parsers
import Annotations

modifier :: Parser String
modifier = classModifier
  <|> accessModifier
  <|> varianceAnnotation
  <|> memberModifier
  <|> parameterModifier
  <|> typeParameterModifier
  <|> functionModifier
  <|> propertyModifier
--

modifiers :: Parser [String]
modifiers = modifier <|> annotations /|\ spaces0P

typeModifiers :: Parser [String]
typeModifiers = suspendModifier <|> annotations /|\ spaces0P

classModifier :: Parser String
classModifier = reservedP "abstract"
  <|> reservedP "final"
  <|> reservedP "enum"
  <|> "open" =>> []
  <|> reservedP "annotation"
  <|> "sealed" =>> "abstract"
  <|> "data" =>> []
--

memberModifier :: Parser String
memberModifier = reservedP "final"
  <|> reservedP "abstract"
  <|> "open" =>> []
  <|> "lateinit" =>> []
  <|> "override" =>> "@override"
--

accessModifier :: Parser String
accessModifier = reservedP "public"
  <|> reservedP "private"
  <|> reservedP "protected"
  <|> "internal" =>> []
--

varianceAnnotation :: Parser String
varianceAnnotation = reservedP "in" <|> reservedP "out"

parameterModifier :: Parser String
parameterModifier = flip convertParserP [] $ reservedP "noinline"
  <|> reservedP "crossinline"
  <|> reservedP "vararg"
--

typeParameterModifier :: Parser String
typeParameterModifier = reservedP "reified"

functionModifier :: Parser String
functionModifier = "tailrec" =>> []
  <|> reservedP "operator"
  <|> "infix" =>> []
  <|> reservedP "inline"
  <|> "external" =>> "/* This is a JNI function */"
  <|> suspendModifier
--

propertyModifier :: Parser String
propertyModifier = reservedP "const"

suspendModifier :: Parser String
suspendModifier = reservedP "suspend"
