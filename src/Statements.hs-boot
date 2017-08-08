module Statements where

import Parsers

statementsP :: Parser String
statementP :: Parser String
jumpP :: Parser String
whenP :: Parser String
whenEntryP :: String -> Parser String
whenConditionP :: String -> Parser String
tryP :: Parser String
catchBlockP :: Parser String
finallyBlockP :: Parser String
whileP :: Parser String
whileHeaderP :: Parser String
doWhileP :: Parser String
loopP :: Parser String
forP :: Parser String
ifP :: Parser String
