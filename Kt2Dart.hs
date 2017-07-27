
module Kt2Dart where

import Control.Applicative

import OperatorP
import Parsers

-- | reference: https://kotlinlang.org/docs/reference/grammar.html
--   Kotlin operator precedence
kotlinOps = parseOperators ops allNameP
  where
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
          , La $ stringP <$>
            [ ".." ]                                 -- range
          , La $ stringP <$>
            [ "+", "-" ]                             -- additive
          , La $ stringP <$>
            [ "*", "/", "%" ]                        -- multiplicative
          , La $ stringP <$>
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
