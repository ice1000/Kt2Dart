module Rules where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types

expressionP :: Parser String
expressionP = reservedP "expr"

statementsP :: Parser String
statementsP = do
  many semiP
  s <- some semiP \|/ statementP
  many semiP
  return $ join s
--

statementP :: Parser String
statementP = reservedP "stmt"

parameterP :: Parser String
parameterP = do
  n <- simpleNameP
  spaces0P
  reservedP ":"
  t <- typeP
  return $ t ++ " " ++ n
--

blockP :: Parser String
blockP = do
  reservedP "{"
  s <- statementsP
  reservedP "}"
  return s
--

valueArgumentsP :: Parser String
valueArgumentsP = do
  i <- bracketsP $ reservedP "," \|/ do
    n <- option0 [] $ do
      n <- simpleNameP
      spaces0P
      reservedP "="
      return $ n ++ "="
    s <- option0 [] $ reservedP "*"
    e <- expressionP
    return $ n ++ s ++ e
  return $ '(' : join i ++ ")"
--
