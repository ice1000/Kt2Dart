module Main where

import Kt2Dart
import Control.Monad

main :: IO ()
main = do
  allCodes <- getContents
  putStrLn allCodes
  putStrLn $ case kotlin2Dart allCodes of
    (Left  o) -> o
    (Right o) -> o
--
