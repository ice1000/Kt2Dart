module Rules where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types
import {-# SOURCE #-} Modifiers

expressionP :: Parser String
expressionP = reservedP "expr"

statementsP :: Parser String
statementsP = do
  many semiP
  s <- option0 [] $ some semiP \|/ statementP
  many semiP
  return $ join s ++ if s /= [] then ";" else []
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
  return $ '{' : s ++ "}"
--

valueArgumentsP :: Parser String
valueArgumentsP = do
  i <- bracketsP $ reservedLP "," \|/ do
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

valueParametersP :: Parser String
valueParametersP = do
  ls <- bracketsP $ option0 [] $ reservedP "," \|/ functionParameterP
  return $ '(' : join ls ++ ")"
--

-- | consider do something to the sencond
--   statement of the do notation
functionParameterP :: Parser String
functionParameterP = do
  m <- option0 [] modifiersP
  reservedP [] <~> reservedP "var" <|> "val" =>> "var"
  p <- parameterP
  e <- reservedP [] <~> do
    reservedP "="
    e <- expressionP
    return $ '=' : e
  return $ m ++ p ++ e
--

