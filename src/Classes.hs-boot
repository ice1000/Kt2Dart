module Classes where

import Parsers

anonymousInitializerP :: Parser String
explicitDelegationP :: Parser String
membersP :: String -> Parser [String]
memberDelarationP :: String -> Parser String
classBodyP :: String -> Parser [String]
classP :: Parser String
primaryConstructorP :: Parser String
delegationSpecifierP :: Parser String
objectP :: Parser String
getterP :: String -> Parser String
setterP :: String -> Parser String
companionObjectP :: Parser (String, [String])
constructorDelegationCallP :: Parser String
propertyP :: Parser String
secondaryConstructerP :: String -> Parser String