module Expressions where

import Parsers

expressionP :: Parser String
blockLevelExpressionP :: Parser String
multiplicativeOperationP :: Parser String
additiveOperationP :: Parser String
isOperationP :: Parser String
typeOperationP :: Parser String
inOperationP :: Parser String
comparisionOperationP :: Parser String
equalityOperationP :: Parser String
assignmentOperatorP :: Parser String
prefixUnaryOperationP :: Parser String
annotatedLambdaP :: Parser String
callSuffixP :: Parser String
postfixUnaryOperationP :: Parser String
memberAccessOperationP :: Parser String
arrayAccessP :: Parser String
constructorInvocationP :: Parser String
postfixUnaryExpressionP :: Parser String
prefixUnaryExpressionP :: Parser String
typeRhsP :: Parser String
multiplicativeExpressionP :: Parser String
additiveExpressionP :: Parser String
disjunctionP :: Parser String
conjunctionP :: Parser String
equalityComparisionP :: Parser String
comparisionP :: Parser String
namedInfix :: Parser String
elvisExpressionP :: Parser String
rangeExpressionP :: Parser String
callableReferenceP :: Parser String
atomicExpressionP :: Parser String
