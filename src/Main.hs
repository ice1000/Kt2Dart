{-# LANGUAGE ApplicativeDo #-}

module Main where

import Kt2Dart
import Control.Monad

import System.IO

main :: IO ()
main = do
  putStrLn "Please specify the input file:"
  file <- getLine
  allCodes <- readFile file
  putStrLn "File contents:"
  putStrLn allCodes
  putStrLn $ case kotlin2Dart allCodes of
    (Left  o) -> "Error:\n" ++ o
    (Right o) -> o
--
