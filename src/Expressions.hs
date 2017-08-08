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
expressionP = disjunctionP <=> assignmentOperatorP

blockLevelExpressionP :: Parser String
blockLevelExpressionP = do
  a <- annotationsP
  e <- expressionP
  return $ a ++ "\n" ++ e
--

multiplicativeOperationP :: Parser String
multiplicativeOperationP = reservedWordsLP [ "*", "/", "%" ]

additiveOperationP :: Parser String
additiveOperationP = reservedWordsLP [ "+", "-" ]

inOperationP :: Parser String
inOperationP = reservedWordsLP [ "in", "!in" ]

typeOperationP :: Parser String
typeOperationP = reservedWordsLP [ "as", "as?", ":" ]

isOperationP :: Parser String
isOperationP = reservedLP "is" <|> "!is" ->> "is!"

comparisionOperationP :: Parser String
comparisionOperationP = reservedWordsLP [ ">=", "<=", "<", ">" ]

equalityOperationP :: Parser String
equalityOperationP = reservedWordsLP [ "!=", "==" ]

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
  <|> "!!" ->> ""
  <|> callSuffixP
  <|> arrayAccessP
  <|> memberAccessOperationP <++> postfixUnaryOperationP
  where
    ops = [ "++", "--" ]
--

callSuffixP :: Parser String
callSuffixP = a <|> b
  where
    a = do
      ta <- option0 [] typeArgumentsP
      va <- valueArgumentsP <++> annotatedLambdaP
      return $ ta ++ va
    b = typeArgumentsP <++> annotatedLambdaP
--

annotatedLambdaP :: Parser String
annotatedLambdaP = do
  ua <- many $ do
    reservedLP "@"
    unescapedAnnotationP
  ld <- option0 [] labelDefinitionP
  fl <- functionLiteralP
  return $ "/* WARNING: annotated lambda " ++ join ua ++ " is not supported */" ++ ld ++ fl
--

memberAccessOperationP :: Parser String
memberAccessOperationP = reservedWordsLP [ ".", "?." ] <|> "?" ->> ""

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
arrayAccessP = do
  reservedLP "["
  e <- reservedLP "," \|/ expressionP
  reservedLP "]"
  return $ '[' : join e ++ "]"
--

constructorInvocationP :: Parser String
constructorInvocationP = userTypeP <++> callSuffixP

postfixUnaryExpressionP :: Parser String
postfixUnaryExpressionP = do
  e <- atomicExpressionP <|> callableReferenceP
  p <- many postfixUnaryOperationP
  return $ e ++ join p
--

prefixUnaryExpressionP :: Parser String
prefixUnaryExpressionP = do
  m <- many prefixUnaryOperationP
  p <- postfixUnaryExpressionP
  return $ join m ++ p
--

typeRhsP :: Parser String
typeRhsP = tokenLP $ prefixUnaryExpressionP <=> typeOperationP

multiplicativeExpressionP :: Parser String
multiplicativeExpressionP = tokenLP $ typeRhsP <=> multiplicativeOperationP

additiveExpressionP :: Parser String
additiveExpressionP = tokenLP $ multiplicativeExpressionP <=> additiveOperationP

disjunctionP :: Parser String
disjunctionP = tokenLP $ conjunctionP </> reservedLP "||"

conjunctionP :: Parser String
conjunctionP = tokenLP $ equalityComparisionP </> reservedLP "&&"

equalityComparisionP :: Parser String
equalityComparisionP = tokenLP $ comparisionP <=> equalityOperationP

comparisionP :: Parser String
comparisionP = tokenLP $ namedInfix <=> comparisionOperationP

namedInfix :: Parser String
namedInfix = b <|> a
  where
    -- b = elvisExpressionP ~> do
    --   inOperationP
    --   return $ \l r -> r ++ ".contains(" ++ l ++ ")"
    b = elvisExpressionP ~~> do
      (i : _) <- inOperationP
      return $ if i == '!'
        then \l r -> '!' : r ++ ".contains(" ++ l ++ ")"
        else \l r -> r ++ ".contains(" ++ l ++ ")"

    a = do
      e <- elvisExpressionP
      m <- option0 [] $ do
        i <- isOperationP
        t <- typeP
        return $ i ++ optionalSuffix t
      return . bracketsHelper_ e $ optionalSuffix m
--

elvisExpressionP :: Parser String
elvisExpressionP = tokenLP $ infixFunctionCallP </> "?:" ->> "??"

infixFunctionCallP :: Parser String
infixFunctionCallP = tokenLP $ rangeExpressionP ~> do
  newLines0P
  n <- simpleNameP
  return $ \l r -> l ++ "." ++ n ++ "(" ++ r ++ ")"
--

-- | I'm not sure how to express this in Dart
rangeExpressionP :: Parser String
rangeExpressionP = additiveExpressionP </> reservedLP ".."
  -- do
  --   e <- additiveExpressionP
  --   m <- many $ do
  --     o <- reservedLP ".."
  --     e <- additiveExpressionP
  --     return $ o ++ e
  --   return $ e ++ join m
--

callableReferenceP :: Parser String
callableReferenceP = do
  t <- option0 [] $ do
    t <- userTypeP
    many $ reservedLP "?"
    return t
  -- there're no such stupid stuff in Dart
  reservedLP "::"
  newLines0P
  n <- simpleNameP
  o <- option0 [] typeArgumentsP
  return $ t ++ n ++ o
--

atomicExpressionP :: Parser String
atomicExpressionP = tokenLP $ bracketedE
  <|> literalConstantP
  <|> functionLiteralP
  <|> thisLabelE
  <|> superLabelE
--  <|> ifP
  <|> whenP
  <|> tryP
--  <|> objectLiteralP
  <|> jumpP
  <|> loopP
  <|> simpleNameP
  where
    bracketedE = do
      e <- bracketsP expressionP
      return $ '(' : e ++ ")"
    thisLabelE = do
      reservedLP "this"
      l <- option0 [] labelReferenceP
      return $ "this" ++ l
    superLabelE = do
      reservedLP "super"
      s <- option0 [] $ do
        reservedLP "<"
        t <- typeP
        reservedLP ">"
        return $ "/* WARNING: refering to a specific super type "
          ++ t ++ " is not supported */"
      l <- option0 [] labelReferenceP
      return $ "super" ++ s ++ l
--
