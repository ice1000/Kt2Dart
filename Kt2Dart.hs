
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
        continue' = do
          stringP "continue"
          label
          spaces0P
          return "continue"
        break' = do
          stringP "break"
          label
          spaces0P
          return "break"
        throw' = do
          reservedP "throw"
          spaces0P
          e <- kotlinExpr
          return $ "throw " ++ e
        return' = do
          stringP "return"
          label
          spacesP
          e <- kotlinExpr
          return $ "return " ++ e
--

kotlinExpr = allNameP

-- | reference: https://kotlinlang.org/docs/reference/grammar.html
--   Kotlin operator precedence
kotlinOps = parseOperators ops kotlinExpr >>= return . flattenTree fa fb
  where
    fa "===" = "=="
    fa "!==" = "!="
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
            [ nameP ]                                -- infix function
          , Na $ stringP <$>
            [ ".." ]                                 -- range
          , La $ stringP <$>
            [ "+", "-" ]                             -- additive
          , La $ stringP <$>
            [ "*", "/", "%" ]                        -- multiplicative
          , Na $ stringP <$>
            [ ":", "as", "as?" ]                     -- type RHS
          ]
--

kotlinParse = undefined

main :: IO ()
main = do
  all' <- getContents
  putStr $ case kotlinParse all' of
    (Just o) -> o
    Nothing  -> "Syntax error."
--
