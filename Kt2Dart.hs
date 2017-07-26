
module Kt2Dart where

import OperatorP
import Parsers

kotlinOps = parseOperators
  [ Na [ string "="
      , string "+="
      , string "-="
      , string "*="
      , string "/="
      , string ">="
      , string "<="
      , string ">"
      , string "<"]
  , La [ string "=="
      , string "!="]
  , La [ string "&&"
      , string "||"]
  , La [string "+", string "-"]
  , La [string "*", string "/"]
  ]

kotlinParse = undefined

main :: IO ()
main = do
  all' <- getContents
  putStr $ case kotlinParse all' of
    (Just o) -> o
    Nothing  -> "Syntax error."
--
