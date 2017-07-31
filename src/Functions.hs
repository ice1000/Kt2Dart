module Functions where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Types

functionP :: Parser String
functionP = do
  reservedP "fun"
  tp <- option0 [] typeParametersP
  rc <- option0 [] $ do
    t <- typeP
    reservedP "."
    return t
  sn <- simpleNameP
  vp <- valueParametersP
  rt <- option0 "" $ do
    reservedP ":"
    t <- typeP
    return $ t ++ " "
  tc <- typeConstraintsP
  fb <- option0 [] functionBodyP
  return $ rt ++ sn ++ tp ++ vp ++ fb
--

functionBodyP :: Parser String
functionBodyP = blockP <~> do
  reservedP "="
  e <- expressionP
  return $ "=>" ++ e
--

