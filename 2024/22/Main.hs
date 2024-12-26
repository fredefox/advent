{-# options_ghc -Wall #-}
module Main where

import Data.Bits

main :: IO ()
main = do
  xs <- fmap (read @Int) . lines <$> getContents
  print $ sum $ ((!! 2000) . iterate step) <$> xs

step :: Int -> Int
step = mult 2048 . div32 . mult 64

mult :: Int -> Int -> Int
mult k n = mixp (k * n) n

div32 :: Int -> Int
div32 n = mixp (n `div` 32) n

mixp :: Int -> Int -> Int
mixp k = (`mod` 16777216) . (`xor` k)
