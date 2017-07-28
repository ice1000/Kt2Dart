module Main where

import Kt2Dart
import Control.Monad

main :: IO ()
main = forever $ do
  putStr "|>"
  all' <- getContents
  putStrLn $ case kotlin2Dart all' of
    (Left  o) -> o
    (Right o) -> "Syntax error:\n" ++ o
--
