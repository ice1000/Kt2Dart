{-# LANGUAGE ApplicativeDo #-}

module Expressions where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import Annotations
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Types
import {-# SOURCE #-} Statements
import {-# SOURCE #-} Strings
import {-# SOURCE #-} Functions

expressionP :: Parser String
expressionP = reservedP "expr"

blockLevelExpressionP :: Parser String
blockLevelExpressionP = do
  a <- annotationsP
  e <- expressionP
  return $ a ++ "\n" ++ e
--

multiplicativeOperationP :: Parser String
multiplicativeOperationP = reservedWordsLP [ "+", "/", "%" ]

additiveOperationP :: Parser String
additiveOperationP = reservedWordsLP [ "+", "-" ]

inOperationP :: Parser String
inOperationP = reservedWordsLP [ "in", "!in" ]

typeOperationP :: Parser String
typeOperationP = reservedWordsLP [ "as", "as?", ":" ]

isOperationP :: Parser String
isOperationP = reservedWordsLP [ "is", "!is" ]

comparisionOperationP :: Parser String
comparisionOperationP = reservedWordsLP [ "!=", "==" ]

assignmentOperatorP :: Parser String
assignmentOperatorP = reservedWordsLP ops
  where
    ops = [ "="
          , "+=", "-=", "*=", "/=", "%="
          ]
--

prefixUnaryOperationP :: Parser String
prefixUnaryOperationP = reservedWordsLP ops
  <|> annotationsP
  <|> labelDefinitionP
  where ops = [ "-", "+"
              , "++", "--"
              , "!"
              ]
--

postfixUnaryOperationP :: Parser String
postfixUnaryOperationP = reservedWordsLP ops
  <|> callSuffixP
  <|> arrayAccessP
  <|> do
  m <- memberAccessOperationP
  p <- postfixUnaryOperationP
  return $ m ++ p
  where
    ops = [ "++", "--", "!!" ]
--

callSuffixP :: Parser String
callSuffixP = a <|> b
  where
    a = do
      ta <- option0 [] typeArgumentsP
      va <- valueArgumentsP
      al <- annotatedLambdaP
      return $ ta ++ va ++ al
    b = do
      ta <- typeArgumentsP
      al <- annotatedLambdaP
      return $ ta ++ al
--

annotatedLambdaP :: Parser String
annotatedLambdaP = do
  ua <- many $ do
    reservedLP "@"
    unescapedAnnotationP
    return "/* WARNING: annotated lambda is not supported */"
  ld <- option0 [] labelDefinitionP
  fl <- functionLiteralP
  return $ f ua ++ ld ++ fl
  where f [     ] = []
        f (a : _) = a
--

memberAccessOperationP :: Parser String
memberAccessOperationP = reservedWordsLP [ ".", "?.", "?" ]

literalConstantP :: Parser String
literalConstantP = reservedWordsLP [ "true", "false", "null" ]
  <|> stringTemplateP
  <|> noEscapeStringP
  <|> integerLiteralP
  <|> hexadecimalLiteralP
  <|> characterLiteralP
  <|> floatLiteralP
--

arrayAccessP :: Parser String
arrayAccessP = undefined
