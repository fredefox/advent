{-# language GHC2024 #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Data.Bits
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  xs <- fmap (read @Int) . lines <$> getContents
  let ys = take 2001 . iterate step <$> xs
  print $ sum $ fmap last ys
  print $ maximum $ mkMaps ys

runsOf :: Int -> [a] -> [[a]]
runsOf n xs = filter (lengthAtLeast 4) $ fmap (take n) $ List.tails xs

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast n = not . null . drop (pred n)

mkMaps :: [[Int]] -> Map [Int] Int
mkMaps = Map.unionsWith (+) . fmap mkMap

mkMap :: [Int] -> Map [Int] Int
mkMap xs
  = Map.fromList
  $ reverse
  $ runsOf 4 (diffOfLastDigits xs) `zip` drop 3 (lastDigits xs)

diffOfLastDigits :: [Int] -> [Int]
diffOfLastDigits xs = zipWith subtract (0 : lastDigits xs) $ lastDigits xs

lastDigits :: [Int] -> [Int]
lastDigits = fmap (`mod` 10)

step :: Int -> Int
step = mult 2048 . div32 . mult 64

mult :: Int -> Int -> Int
mult k n = mixp (k * n) n

div32 :: Int -> Int
div32 n = mixp (n `div` 32) n

mixp :: Int -> Int -> Int
mixp k = (`mod` 16777216) . (`xor` k)
