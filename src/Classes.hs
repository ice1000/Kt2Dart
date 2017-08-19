{-# LANGUAGE ApplicativeDo #-}

module Classes where

import Control.Applicative
import Control.Monad

import Parsers
import Annotations
import LexicalStructure
import {-# SOURCE #-} Rules
import {-# SOURCE #-} Functions
import {-# SOURCE #-} Expressions
import {-# SOURCE #-} Types
import {-# SOURCE #-} Modifiers

anonymousInitializerP :: Parser String
anonymousInitializerP = do
  reservedLP "init"
  blockP
--

-- | should deal with statics
classBodyP :: String -> Parser [String]
classBodyP n = do
  reservedLP "{"
--  newLines0P
  m <- membersP n
  reservedLP "}"
  return m
--

-- | should deal with statics
membersP :: String -> Parser [String]
membersP = many . memberDelarationP

companionObjectP :: Parser (String, [String])
companionObjectP = do
  m <- modifiersP
  reservedLP "companion"
  reservedLP "object"
  newLines0P
  n <- option0 "Companion" simpleNameP
  d <- option0 [] $ do
    reservedLP ":"
    n <- reservedLP "," \|/ delegationSpecifierP
    return $ join n
  b <- classBodyP n
  -- TODO
  -- 23333
  return (
    "/* WARNING: companion object "
      ++ n ++ ":" ++ d
      ++ " is converted into static methods */", b)
--

memberDelarationP :: String -> Parser String
memberDelarationP n = tokenLP $ do
  a <- typeAliasP
    <|> functionP
    <|> coP
    <|> anonymousInitializerP
    <|> objectP
    <|> classP
    <|> propertyP
    <|> secondaryConstructerP n
  option0 ' ' semiP
  return $ a ++ ";"
  where
    coP = do
      (cs, ms) <- companionObjectP
      return $ cs
        ++ join [ "static " ++ e | e <- ms ]
--

objectP :: Parser String
objectP = do
  reservedLP "object"
  newLines0P
  n <- simpleNameP
  c <- primaryConstructorP
  d <- option0 [] $ do
    reservedLP ":"
    n <- reservedLP "," \|/ delegationSpecifierP
    return $ join n
  b <- classBodyP n
  -- TODO
  return $ "class " ++ n
    ++ join [ "static " ++ e | e <- b ]
--

debug = reservedLP []

classP :: Parser String
classP = do
  m <- modifiersP
  reservedWordsLP [ "class", "interface" ]
  newLines0P -- necessary
  n <- simpleNameP
  t <- option0 [] typeParametersP
  p <- option0 [] primaryConstructorP
  e <- option0 [] $ do
    reservedLP ":"
    a <- option0 [] annotationsP
    d <- reservedLP "," \|/ delegationSpecifierP
    return $ " extends " ++ a ++ join d
  c <- typeConstraintsP
  s <- option0 [] $
    classBodyP n <|> enumClassBodyP
  return $ optionalSuffix m ++ "class " ++ n ++ t
    ++ e ++ c ++ case s of
          [] -> '{' : p ++ "}"
          ls -> '{' : (join $ p : ls) ++ "}"
--

enumClassBodyP :: Parser [String]
enumClassBodyP = return <$> reservedLP "enumClassBody"

-- | maybe I just need the parameters
primaryConstructorP :: Parser String
primaryConstructorP = do
  m <- option0 [] $ do
    m <- modifiersP
    reservedLP "constructor"
    return m
  b <- bracketsP $
    reservedLP "," \|/ functionParameterP
  return $ join b
--

delegationSpecifierP :: Parser String
delegationSpecifierP = constructorInvocationP
  <|> explicitDelegationP
  <|> userTypeP
--

explicitDelegationP :: Parser String
explicitDelegationP = do
  t <- userTypeP
  reservedLP "by"
  e <- expressionP
  return $ "/* WARNING: delegation "
    ++ e ++ " to " ++ t ++ " is not supported */"
--

-- | The name of the property should be given
getterP :: String -> Parser String
getterP n = do
  m <- modifiersP
  reservedLP "get"
  g <- option0 ([], []) $ do
    bracketsP newLines0P
    t <- option0 "dynamic" $ do
      reservedLP ":"
      typeP
    b <- functionBodyP
    return (t, b)
  return $ case g of
    ([], []) -> []
    (t , b ) -> t ++ " get "
      ++ n ++ processBody b
--

processBody :: String -> String
processBody b = case head b of
  '=' -> b ++ ";"
  _   -> "()" ++ b ++ "()"
--

setterP :: String -> Parser String
setterP n = do
  m <- modifiersP
  reservedLP "set"
  g <- option0 ([], []) $ do
    p <- bracketsP $
      modifiersP <++> parameterP <|> simpleNameP
    b <- functionBodyP
    return (p, b)
  return $ case g of
    ([], []) -> []
    (p , b ) -> "set " ++ n
      ++ "(" ++ p ++ ")" ++ processBody p
--

constructorDelegationCallP :: Parser String
constructorDelegationCallP = a <++> valueArgumentsP
  where a = reservedWordsLP [ "this", "super" ]
--

propertyP :: Parser String
propertyP = do
  md <- modifiersP
  reservedWordsLP [ "var", "val" ]
  tp <- option0 [] typeParametersP
  et <- option0 [] $ do
    t <- typeP
    reservedLP "."
    return $ "/* extension property to "
      ++ t ++ " is not supported */"
  (n, t) <- variableDeclarationEntrySP <|> mtp
  tc <- typeConstraintsP
  by <- option0 [] $ do
    w <- reservedWordsLP [ "by", "=" ]
    e <- expressionP
    semiP
    return $ case w of
      "=" -> '=' : e
      _   -> "/* delegation property by "
        ++ e ++ " is not supported */"
  gs <- getterP n <|> setterP n
  sg <- getterP n <|> setterP n
  option0 ' ' semiP
  return $ case by of
    [] -> gs ++ sg
    by -> md ++ t ++ et ++ n ++ by ++ gs ++ sg
  where
    mtp = do
      q <- multipleVariableDeclarationsP
      return ([], "/* multiple variable declaration "
        ++ q ++ " is not supported */")
--

secondaryConstructerP :: String -> Parser String
secondaryConstructerP n = do
  m <- modifiersP
  reservedLP "constructor"
  p <- valueArgumentsP
  c <- option0 [] $ do
    reservedLP ":"
    constructorDelegationCallP
  (h : t) <- blockP
  return $ n ++ p ++ [ h ] ++ c ++ ";" ++ t
--
