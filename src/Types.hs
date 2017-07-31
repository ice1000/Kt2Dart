module Types where

import Control.Applicative
import Control.Monad

import Parsers
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Rules

typeP :: Parser String
typeP = undefined

typeArgumentsP :: Parser String
typeArgumentsP = do
  charP '<'
  ts <- typeP /|\ stringP ","
  return ""
--

