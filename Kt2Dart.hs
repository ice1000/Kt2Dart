
module Kt2Dart where

import Control.Applicative

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
  s <- kotlinJumps <|> kotlinCallExpr <|> kotlinOps
  return $ s ++ ";"
--

kotlinCallExpr = do
  n <- kotlinExpr
  reservedP "("
  e <- kotlinExpr
  reservedP ")"
  return $ n ++ "(" ++ e ++ ")"
--

kotlinExpr = kotlinOps

-- | reference: https://kotlinlang.org/docs/reference/grammar.html
--   Kotlin operator precedence
kotlinOps = parseOperators ops allNameP >>=
            return . flattenTree fa fb
  where
    fa "===" = "=="
    fa "!==" = "!="
    fa "?:"  = "??"
    fa other = other
    fb       = id
    ops = [ La $ stringP <$>
            [ "," ]                                  -- special
          , Na $ stringP <$>
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
            [ ".", "?." ]                            -- member access
          ]
--

kotlin2Dart = undefined

main :: IO ()
main = do
  all' <- getContents
  putStr $ case kotlin2Dart all' of
    (Left  o) -> o
    (Right o) -> "Syntax error:\n" ++ o
--
