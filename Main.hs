module Main where

import Kt2Dart
import Control.Monad

main :: IO ()
main = do
  forever $ do
    all' <- getLine
    putStrLn $ case kotlin2Dart all' of
      (Left  o) -> o
      (Right o) -> "Syntax error:\n" ++ o
--
