{-# LANGUAGE ApplicativeDo #-}

module Annotations where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types
import {-# SOURCE #-} Rules

annotationsP :: Parser String
annotationsP = do
  s <- newLines0P \|/ annotationP <|> annotationListP
  newLines0P
  return $ join s
--

annotationP :: Parser String
annotationP = do
  reservedP "@"
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    reservedLP ":"
    return $ a ++ ":"
  u <- unescapedAnnotationP
  return $ '@' : s ++ u
--

annotationListP :: Parser String
annotationListP = do
  reservedP "@"
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    reservedLP ":"
    return []
  reservedLP "["
  u <- some unescapedAnnotationP
  reservedLP "]"
  return $ '@' : s ++ join u
--

annotationUseSiteTargetP :: Parser String
annotationUseSiteTargetP = reservedWordsLP words
  where
    words = [ "field"
            , "file"
            , "property"
            , "get"
            , "set"
            , "reciever"
            , "param"
            , "setparam"
            , "delegate"
            ]
--

unescapedAnnotationP :: Parser String
unescapedAnnotationP = do
  ns <- reservedLP "." \|/ tokenP simpleNameP
  ta <- option0 [] typeArgumentsP
  va <- option0 [] valueArgumentsP
  return $ join ns ++ ta ++ va
--
