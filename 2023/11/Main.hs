{-# language Haskell2010, LambdaCase #-}
module Main (main) where

import qualified Data.List as List
import Data.Array (Array)
import qualified Data.Array as Array
import Control.Monad
import Data.Coerce

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = expand xs
  let gs = fmap fst $ filter (\(_, c) -> c /= '.') $ Array.assocs $ matrix m
  print $ sum $ uncurry dist <$> pairs gs

pairs :: [a] -> [(a, a)]
pairs = \case
  [] -> []
  (x:xs) -> ((,) x <$> xs) <> pairs xs

dist :: Num a => (a, a) -> (a, a) -> a
(a0, b0) `dist` (a1, b1) = abs (a0 - a1) + abs (b0 - b1)

expand :: [String] -> [String]
expand = List.transpose . foldMap go . List.transpose . foldMap go
  where
  go :: String -> [String]
  go xs | all (== '.') xs = [xs, xs]
        | otherwise = [xs]

matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x
