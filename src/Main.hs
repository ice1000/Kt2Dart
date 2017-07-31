module Main where

import Kt2Dart
import Control.Monad

main :: IO ()
main = do
  all' <- getContents
  putStrLn $ case kotlin2Dart all' of
    (Left  o) -> o
    (Right o) -> "Result:\n\n" ++ o
--
