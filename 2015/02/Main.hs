module Main where

import Data.Foldable
import Data.List

main :: IO ()
main = do
  xs <- fmap sort . parse <$> getContents
  print $ sum $ solve <$> xs
  print $ sum $ ribbon <$> xs
parse :: String -> [[Integer]]
parse = fmap (fmap read . words) . lines

solve :: Num a => [a] -> a
solve (l:w:h:_) = area + slack
  where
  area = 2 * (l * w + w * h + h * l)
  slack = l * w

ribbon :: Num a => [a] -> a
ribbon (l:w:h:_) = 2 * (l + w) + l * w * h
