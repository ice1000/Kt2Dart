{-# LANGUAGE ApplicativeDo #-}

module Modifiers where

import Control.Applicative
import Control.Monad

import Parsers
import Annotations

modifierP :: Parser String
modifierP = foldr1 (<|>) ms
  where
    ms = [ classModifierP
         , accessModifierP
         , varianceAnnotationP
         , memberModifierP
         , parameterModifierP
         , typeParameterModifierP
         , functionModifierP
         , propertyModifierP
         ]
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
classModifierP = reservedWordsLP w
  <|> "sealed" ->> "abstract"
  <|> "data" ->> []
  <|> "open" ->> []
  where
    w =  [ "abstract"
         , "final"
         ,"enum"
         , "annotation"
         ]
--

memberModifierP :: Parser String
memberModifierP = reservedWordsLP w 
  <|> "open" ->> []
  <|> "lateinit" ->> []
  <|> "override" ->> "@override"
  where
    w = [ "final"
        , "abstract"
        ]
--

accessModifierP :: Parser String
accessModifierP = reservedLP "public"
  <|> reservedLP "private"
  <|> reservedLP "protected"
  <|> "internal" ->> []
--

varianceAnnotationP :: Parser String
varianceAnnotationP = reservedWordsLP [ "in", "out" ]

parameterModifierP :: Parser String
parameterModifierP = flip convertParserP [] $ reservedWordsLP w
  where
    w = [ "noinline"
        , "crossinline"
        , "vararg"
        ]
--

typeParameterModifierP :: Parser String
typeParameterModifierP = reservedLP "reified"

functionModifierP :: Parser String
functionModifierP = "tailrec" ->> []
  <|> reservedLP "operator"
  <|> "infix" ->> []
  <|> reservedLP "inline"
  <|> "external" ->> "/* WARNING: This is a JNI function */"
  <|> suspendModifierP
--

propertyModifierP :: Parser String
propertyModifierP = reservedLP "const"

suspendModifierP :: Parser String
suspendModifierP = reservedLP "suspend"
