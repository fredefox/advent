{-# language TypeApplications #-}
import qualified Data.List

import Data.Bits
import qualified Data.List
-- import Debug.Trace

main :: IO ()
main = do
  xs <- go <$> getContents
  print $ solve1 xs
  print $ solve2 (pred $ length $ head xs) $ fromBin <$> xs
  where
  go :: String -> [[Int]]
  go = fmap (fmap (read @Int . pure)) . lines

fromBin :: [Int] -> Int
fromBin xs = sum $ zipWith shift (reverse xs) [0..]

solve1 :: [[Int]] -> Int
solve1 xs = a * b
  where
  bits = f <$> Data.List.transpose xs
  b = fromBin bits
  a = fromBin $ (\n -> (n + 1) `mod` 2) <$> bits
  p n x = length $ filter (== n) x
  f x = fromEnum @Bool $ p 0 x < p 1 x

solve2 :: Int -> [Int] -> Int
solve2 n xs = a * b
  where
  a = solve2' (<) n xs
  b = solve2' (>=) n xs

solve2' :: (Int -> Int -> Bool) -> Int -> [Int] -> Int
solve2' _ _ [x] = x
solve2' p i xs = solve2' p (pred i) $ if length b0 `p` length b1 then b0 else b1
  where
  b0 = filter (`testBit` i) xs
  b1 = filter (not . (`testBit` i)) xs
