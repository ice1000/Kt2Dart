module Rules where

import Parsers

parameterP :: Parser String
blockP :: Parser String
valueArgumentsP :: Parser String
valueParametersP :: Parser String
functionParameterP :: Parser String
labelReferenceP :: Parser String
labelDefinitionP :: Parser String
lambdaParameterP :: Parser String
controlStructureBodyP :: Parser String
multipleVariableDeclarationsP :: Parser String
variableDeclarationEntryP :: Parser String
variableDeclarationEntrySP :: Parser (String, String)