module Main (main) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  inp <- parse <$> getContents
  let (xs, ys) = unzip inp
  print $ part1 xs ys
  print $ part2 xs ys

part1 :: (Num a, Ord a) => [a] -> [a] -> a
part1 xs ys = sum $ zipWith dist (List.sort xs) (List.sort ys)

part2 :: (Foldable t, Functor t) => t Int -> [Int] -> Int
part2 xs ys = sum $ fmap go xs
  where
  m :: Map Int Int
  m = Map.fromListWith (+) $ (\y -> (y, 1)) <$> ys
  go :: Int -> Int
  go = (\x -> fromMaybe 0 (Map.lookup x m) * x)

dist :: Num a => a -> a -> a
dist a b = abs $ a - b

parse :: String -> [(Int, Int)]
parse = fmap ((\(a:b:_) -> (a, b)) . fmap (read @Int) . words) . lines
