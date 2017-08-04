{-# LANGUAGE ApplicativeDo #-}

module Functions where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Types

functionP :: Parser String
functionP = do
  reservedP "fun"
  tp <- option0 [] typeParametersP
  rc <- option0 [] $ do
    t <- typeP
    reservedLP "."
    return t
  sn <- simpleNameP
  vp <- valueParametersP
  rt <- option0 [] $ do
    reservedLP ":"
    t <- typeP
    return $ t ++ " "
  tc <- typeConstraintsP
  fb <- option0 [] functionBodyP
  newLines0P
  return $ rt ++ sn ++ tp ++ vp ++ fb
--

functionBodyP :: Parser String
functionBodyP = blockP <~> do
  reservedLP "="
  e <- expressionP
  return $ "=>" ++ e
--

