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
stringTemplateElementP = regularStringPartP
   <|> shortTemplateEmtryStartP
   <|> escapeSequenceP
   <|> longTemplateP
--

longTemplateP :: Parser String
longTemplateP = do
  reservedLP "${"
  e <- expressionP
  charP '}'
  return $ "${" ++ e ++ "}"
--
