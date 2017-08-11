module Classes where

import Parsers

anonymousInitializerP :: Parser String
explicitDelegationP :: Parser String
membersP :: Parser [String]
memberDelarationP :: Parser String
classBodyP :: Parser [String]
primaryConstructorP :: Parser String
delegationSpecifierP :: Parser String
objectP :: Parser String
getterP :: Parser String
companionObjectP :: Parser (String, [String])