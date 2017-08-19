{-# LANGUAGE ApplicativeDo #-}

module Annotations where

import Control.Applicative
import Control.Monad

import Data.Char

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types
import {-# SOURCE #-} Rules

annotationsP :: Parser String
annotationsP = do
  s <- some . tokenLP $ annotationP <|> annotationListP
  return $ join s
--

-- | convert "Override" into "override"
--   "Deprecated" into "deprecated"
annotationP :: Parser String
annotationP = do
  reservedLP "@"
  s <- option0 [] $ do
    a <- annotationUseSiteTargetP
    reservedLP ":"
    return $ a ++ ":"
  u <- unescapedAnnotationP
  return $ '@' : s ++ mapped u
  where mapped [     ] = []
        mapped (h : t) = toLower h : t
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
  ns <- reservedLP "." \|/ tokenLP simpleNameP
  ta <- option0 [] typeArgumentsP
  va <- option0 [] valueArgumentsP
  return $ join ns ++ ta ++ va
--
