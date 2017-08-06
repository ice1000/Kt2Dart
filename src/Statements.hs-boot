module Statements where

import Parsers

statementsP :: Parser String
statementP :: Parser String
jumpP :: Parser String
whenP :: Parser String
whenEntryP :: String -> Parser String
whenConditionP :: String -> Parser String