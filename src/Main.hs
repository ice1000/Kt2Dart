module Main where

import Kt2Dart
import Control.Monad

import System.IO

doRead fn op = do
  h <- openFile fn ReadMode
  op <$> hGetContents h
  hClose h
--

main :: IO ()
main = do
  putStrLn "Please specify the input file:"
  file <- getLine
  doRead file $ \allCodes ->
    putStrLn $ case kotlin2Dart allCodes of
      (Left  o) -> o
      (Right o) -> o
--
