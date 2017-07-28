
module Kt2Dart where

import Control.Applicative
import Control.Monad

import OperatorP
import Parsers

kotlinJumps :: Parser String
kotlinJumps = continue' <|> throw' <|> break' <|> return'
  where label = option0 "" $ do
          charP '@'
          some $ oneOf alpha
          return ""
        f s = do
          stringP s
          label
          spaces0P
          return s
        continue' = f "continue"
        break' = f "break"
        throw' = do
          stringP "throw"
          spacesP
          e <- kotlinExpr
          return $ "throw " ++ e
        return' = do
          stringP "return"
          label
          spacesP
          e <- kotlinExpr
          return $ "return " ++ e
--

kotlinStatement :: Parser String
kotlinStatement = do
  s <- kotlinJumps <|> kotlinCallExpr <|> kotlinExpr
  option0 "" $ reservedP ";"
  return $ s ++ ";"
--

kotlinCallExpr = do
  n <- kotlinExpr
  e <- return [] <~> do
    reservedP "("
    e <- option0 [] $ seperateP kotlinExpr ","
    reservedP ")"
    return $ join e
  l <- option0 "" kotlinLambda
  return $ n ++ "(" ++ f e l ++ ")"
  where f a@(_ : _) b@(_ : _) = a ++ "," ++ b
        f a         b         = a ++ b
--

kotlinExpr = kotlinOps <|> kotlinLambda

kotlinLambda :: Parser String
kotlinLambda = do
  reservedP "{"
  pm <- option0 [] $ do
    p <- seperateP nameP ","
    reservedP "->"
    return p
  stmt <- many kotlinStatement
  reservedP "}"
  return $ "(" ++ join pm ++ "){" ++ join stmt ++ "}"
--

-- | reference: https://kotlinlang.org/docs/reference/grammar.html
--   Kotlin operator precedence
kotlinOps = flattenTree fa fb `liftM` parseOperators ops allNameP
  where
    fa "===" = "=="
    fa "!==" = "!="
    fa "?:"  = "??"
    fa "!!." = "."
    fa other = other
    fb       = id
    ops = [ Na $ stringP <$>
            [ "=", "+=", "-=", "*=", "/=", "%=" ]    -- assignment
          , La $ stringP <$>
            [ "||" ]                                 -- disjunction
          , La $ stringP <$>
            [ "&&" ]                                 -- conjunction
          , La $ stringP <$>
            [ "===", "!==", "==", "!=" ]             -- equality
          , Na $ stringP <$>
            [ ">=", "<=", ">", "<"]                  -- comparison
          , Na $ stringP <$>
            [ "!in", "in", "is", "!is" ]             -- named checks
          , La $ stringP <$>
            [ "?:" ]                                 -- elvis
          , La
            [ do
                n <- nameP
                return $ if n == "in" || n == "is"
                  then n else '#' : n ]              -- infix function
          , Na $ stringP <$>
            [ ".." ]                                 -- range
          , La $ stringP <$>
            [ "+", "-" ]                             -- additive
          , La $ stringP <$>
            [ "*", "/", "%" ]                        -- multiplicative
          , Na $ stringP <$>
            [ ":", "as", "as?" ]                     -- type RHS
          , La $ stringP <$>
            [ ".", "?.", "!!." ]                     -- member access
          ]
--

kotlin2Dart = parseCode kotlinStatement
