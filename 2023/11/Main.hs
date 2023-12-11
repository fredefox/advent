{-# language Haskell2010, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.List as List
import Data.Array (Array)
import qualified Data.Array as Array
import Control.Monad

main :: IO ()
main = do
  xs <- lines <$> getContents
  let gs = fmap fst $ filter (\(_, c) -> c /= '.') $ Array.assocs $ matrix xs
  print $ solve 1 xs gs
  print $ solve (pred 1000000) xs gs

solve :: Int -> [String] -> [(Int, Int)] -> Int
solve k xs gs = sum $ uncurry dist <$> pairs gs'
  where
  row = deltas k xs
  col = deltas k $ List.transpose xs
  gs' = (\(i, j) -> (row i, col j)) <$> gs

deltas :: Int -> [String] -> Int -> Int
deltas k xs i = i + a Array.! i
  where
  go n s = if all (== '.') s then n + k else n
  a = listArray $ scanl go 0 xs

listArray :: [a] -> Array Int a
listArray xs = Array.listArray (0, pred $ length xs) xs

pairs :: [a] -> [(a, a)]
pairs = \case
  [] -> []
  (x:xs) -> ((,) x <$> xs) <> pairs xs

dist :: Num a => (a, a) -> (a, a) -> a
(a0, b0) `dist` (a1, b1) = abs (a0 - a1) + abs (b0 - b1)

matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x
