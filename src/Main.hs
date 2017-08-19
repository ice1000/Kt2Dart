{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad

import System.IO

import Parsers
import Kotlin

main :: IO ()
main = do
  putStrLn "Please specify the input file:"
  file <- getLine
  allCodes <- readFile file
  putStrLn "File contents:"
  putStrLn allCodes
  putStrLn $ case p <|| allCodes of
    (Left  o) -> o
    (Right o) -> o
  where p = do
          f <- kotlinFileP
          newLines0P
          return f
--
