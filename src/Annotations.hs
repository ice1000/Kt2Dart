module Annotations where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types
import {-# SOURCE #-} Rules

annotationsP :: Parser String
annotationsP = do
  s <- spaces0P \|/ annotationP <|> annotationListP
  return $ join s
--

annotationP :: Parser String
annotationP = do
  reservedP "@"
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    reservedP ":"
    return $ a ++ ":"
  u <- unescapedAnnotationP
  return $ '@' : s ++ u
--

annotationListP :: Parser String
annotationListP = do
  reservedP "@"
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    reservedP ":"
    return []
  reservedP "["
  u <- some unescapedAnnotationP
  reservedP "]"
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
  ns <- reservedP "." \|/ tokenP simpleNameP
  ta <- option0 [] typeArgumentsP
  va <- option0 [] valueArgumentsP
  return $ join ns ++ ta ++ va
--
