{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad

import System.IO

import Parsers
import Kotlin

main :: IO ()
main = do
  putStrLn "Please specify the input file:"
  file <- return "tests/types.kt" -- getLine
  allCodes <- readFile file
  putStrLn "File contents:"
  putStrLn allCodes
  putStrLn . show $ p <!-- allCodes
  where p = do
          f <- kotlinFileP
          newLines0P
          return f
--
