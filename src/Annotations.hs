module Annotations where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types
import {-# SOURCE #-} Rules

annotationsP :: Parser String
annotationsP = annotationP <|> annotationListP

annotationP :: Parser String
annotationP = do
  charP '@'
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    charP ':'
    return $ a ++ ":"
  u <- unescapedAnnotationP
  return $ '@' : s ++ u
--

annotationListP :: Parser String
annotationListP = do
  charP '@'
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    charP ':'
    return []
  charP '['
  u <- some unescapedAnnotationP
  charP ']'
  return $ '@' : s ++ join u
--

annotationUseSiteTargetP :: Parser String
annotationUseSiteTargetP = reservedP "field"
  <|> reservedP "file"
  <|> reservedP "property"
  <|> reservedP "get"
  <|> reservedP "set"
  <|> reservedP "reciever"
  <|> reservedP "param"
  <|> reservedP "setparam"
  <|> reservedP "delegate"
--

unescapedAnnotationP :: Parser String
unescapedAnnotationP = do
  ns <- stringP "." \|/ tokenP simpleNameP
  ta <- option0 [] typeArgumentsP
  va <- option0 [] valueArgumentsP
  return $ join ns ++ ta ++ va
--
