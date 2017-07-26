
module Kt2Dart where

import OperatorP

kotlinParse = undefined

main :: IO ()
main = do
  all' <- getContents
  putStr $ case kotlinParse all' of
    (Just o) -> o
    Nothing  -> "Syntax error."
--
