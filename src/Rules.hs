{-# LANGUAGE ApplicativeDo #-}

module Rules where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Types
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Statements

parameterP :: Parser String
parameterP = do
  n <- simpleNameP
  reservedLP ":"
  t <- typeP
  return $ t ++ " " ++ n
--

blockP :: Parser String
blockP = do
  reservedLP "{"
  s <- statementsP
  reservedLP "}"
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
  ls <- bracketsP $ option0 [] $
    reservedLP "," \|/ functionParameterP
  return $ '(' : join ls ++ ")"
--

-- | consider do something to the sencond
--   statement of the do notation
functionParameterP :: Parser String
functionParameterP = do
  m <- option0 [] modifiersP
  reservedLP [] <~> reservedP "var" <|> "val" ->> "var"
  p <- parameterP
  e <- reservedP [] <~> do
    reservedLP "="
    e <- expressionP
    return $ '=' : e
  return $ m ++ p ++ e
--

-- | Here's another issue
--   There is a duplicate `charP '@'` in the doc
--   converted into warnings
labelReferenceP :: Parser String
labelReferenceP = labelNameP

-- | Here's another another issue
--   There is a duplicate `charP '@'` in the doc
--   converted into warnings, too
labelDefinitionP :: Parser String
labelDefinitionP = labelNameSP

theTypedSP :: Parser String -> Parser (String, String)
theTypedSP s = do
  n <- s
  o <- option0 [] $ do
    reservedLP ":"
    typeP
  return (o, n)
--

theTypedP :: Parser String -> Parser String
theTypedP s = do
  (o, n) <- theTypedSP s
  return $ f o ++ n
  where f [] = []
        f ls = ls ++ " "
--

lambdaParameterP :: Parser String
lambdaParameterP = tokenLP $ variableDeclarationEntryP
  <|> theTypedP multipleVariableDeclarationsP
--

variableDeclarationEntryP :: Parser String
variableDeclarationEntryP = theTypedP simpleNameP

variableDeclarationEntrySP :: Parser (String, String)
variableDeclarationEntrySP = theTypedSP simpleNameP

multipleVariableDeclarationsP :: Parser String
multipleVariableDeclarationsP = do
  reservedLP "("
  l <- reservedLP "," \|/ variableDeclarationEntryP
  reservedLP ")"
  return $ "/* WARNING: destructing declaration "
    ++ join l ++ " is not supported */"
--

controlStructureBodyP :: Parser String
controlStructureBodyP = blockP <|> blockLevelExpressionP
