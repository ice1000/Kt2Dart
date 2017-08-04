{-# LANGUAGE ApplicativeDo #-}

module Classes where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Rules

anonymousInitializerP :: Parser String
anonymousInitializerP = do
  reservedLP "init"
  blockP
--


