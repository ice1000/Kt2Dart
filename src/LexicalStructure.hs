{-# LANGUAGE ApplicativeDo #-}

module LexicalStructure where

import Control.Applicative
import Control.Monad

import Parsers

alpha :: String
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

digit :: String
digit = ['0' .. '9']

hexDigitP :: Parser Char
hexDigitP = oneOf $ digit ++ ['a' .. 'f'] ++ ['A' .. 'F']

integerLiteralP :: Parser String
integerLiteralP = do
  t <- some $ digitP <|> charP '_'
  return [ c | c <- t, c /= '_' ]
--

floatLiteralP :: Parser String
floatLiteralP = do
  i <- integerLiteralP
  charP '.'
  r <- integerLiteralP
  -- charP 'F' <|> charP 'f'
  return $ i ++ "." ++ r
--

hexadecimalLiteralP :: Parser String
hexadecimalLiteralP = do
  stringP "0x"
  t <- some $ hexDigitP <|> charP '_'
  return $ "0x" ++ [ c | c <- t, c /= '_' ]
--

characterLiteralP :: Parser String
characterLiteralP = do
  charP '\''
  c <- oneCharP
  charP '\''
  return ['\'', c, '\'']
--

noEscapeStringP :: Parser String
noEscapeStringP = do
  stringP q
  r <- pa
  return $ q ++ r
  where q = "\"\"\""
        pa = stringP q <|> do
          c <- exceptCharP '\"'
          r <- pa
          return $ c : r
--

regularStringPartP :: Parser String
regularStringPartP = do
  c <- noneOf "\\\r\n\"$"
  return [c]
--

shortTemplateEmtryStartP :: Parser String
shortTemplateEmtryStartP = stringP "$"

escapeSequenceP :: Parser String
escapeSequenceP = unicodeEscapeSequeceP <|> regularEscapeSequenceP

unicodeEscapeSequeceP :: Parser String
unicodeEscapeSequeceP = do
  stringP "\\u"
  a <- hexDigitP
  b <- hexDigitP
  c <- hexDigitP
  d <- hexDigitP
  return $ "\\u{" ++ [a, b, c, d] ++ "}"
--

regularEscapeSequenceP :: Parser String
regularEscapeSequenceP = do
  c <- noneOf "\n"
  return [c]
--

semiP :: Parser Char
semiP = do
  charP ';' <|> charP '\n'
  spaces0P
  return ';'
--

javaIdentifierP :: Parser String
javaIdentifierP = do
  c <- oneOf $ "_$" ++ alpha
  r <- many $ oneOf $ "_$" ++ alpha ++ digit
  return $ c : r
--

simpleNameP :: Parser String
simpleNameP = javaIdentifierP <|> do
  charP '`'
  i <- javaIdentifierP
  charP '`'
  return i
--

labelNameP :: Parser String
labelNameP = do
  charP '@'
  ji <- javaIdentifierP
  return $ "/* WARNING: label usage "
    ++ ji ++ " is not supported */"
--

labelNameSP :: Parser String
labelNameSP = do
  ji <- javaIdentifierP
  charP '@'
  return $ "/* WARNING: label definition "
    ++ ji ++ " is not supported */"
--
