{-# LANGUAGE ApplicativeDo #-}

module Strings where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Expressions

stringTemplateP :: Parser String
stringTemplateP = do
  charP '"'
  e <- option0 [] $ some stringTemplateElementP
  reservedLP [ '"' ]
  return $ '"' : join e ++ [ '"' ]
--

stringTemplateElementP :: Parser String
stringTemplateElementP = longTemplateP
  <|> shortTemplateEmtryStartP
  <|> escapeSequenceP
  <|> regularStringPartP
--

longTemplateP :: Parser String
longTemplateP = do
  stringP "${"
  e <- expressionP
  newLines0P
  charP '}'
  return $ "${" ++ e ++ "}"
--
