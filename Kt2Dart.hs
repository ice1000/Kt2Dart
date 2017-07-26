
module Kt2Dart where

import Control.Applicative

import OperatorP
import Parsers

kotlinOps = parseOperators ops term
  where
    ops = [ Na [ stringP "="
               , stringP "+="
               , stringP "-="
               , stringP "*="
               , stringP "/="
               , stringP ">="
               , stringP "<="
               , stringP ">"
               , stringP "<"]
          , La [ stringP "=="
               , stringP "!="]
          , La [ stringP "&&"
               , stringP "||"]
          , La [stringP "+", stringP "-"]
          , La [stringP "*", stringP "/"]
          ]
    term = numberP <|> nameP
--

kotlinParse = undefined

main :: IO ()
main = do
  all' <- getContents
  putStr $ case kotlinParse all' of
    (Just o) -> o
    Nothing  -> "Syntax error."
--
