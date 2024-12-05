module Main (main) where

import Data.List
import Data.Foldable
import Data.Char

main :: IO ()
main = do
  xs <- getContents
  let n = foldr go 0 xs
  print n
  print $ length $ takeWhile (>= 0) $ scanl (flip go) 0 xs
  where
  go '(' = succ
  go ')' = pred
  go _ = id
