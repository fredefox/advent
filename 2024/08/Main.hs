{-# language GHC2021, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main, groupOn, showMatrixWith) where

import Data.Array
import qualified Data.Array as Array
import Control.Monad
import Data.Function
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = matrix xs
  let w = makeMap (/= '.') m
  print $ solve m w

solve :: Foldable t => Array (Int, Int) e -> t [(Int, Int)] -> Int
solve m w
  = Set.size
  $ Set.fromList
  $ foldMap (expand (takeWhile (inBounds m)))
  $ foldMap pairs w

expand :: ([(Int, Int)] -> [(Int, Int)]) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
expand p (a, b) = alternate a' b'
  where
  d = dist a b
  a' = p $ iterate (`plus` d) a
  b' = p $ iterate (`minus` d) b

inBounds :: Ix a => Array a e -> a -> Bool
m `inBounds` ix = Array.bounds m `Array.inRange` ix

alternate :: [a] -> [a] -> [a]
alternate (x:xs) (y:ys) = x : y : alternate xs ys
alternate xs ys = xs <> ys

minus, plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a0, a1) `minus` (b0, b1) = (a0 - b0, a1 - b1)
(a0, a1) `plus` (b0, b1)  = (a0 + b0, a1 + b1)

dist :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
dist (a0, a1) (b0, b1) = (a0 - b0, a1 - b1)

pairs :: [a] -> [(a, a)]
pairs = \case
  [] -> []
  (x:xs) -> ((x,) <$> xs) <> pairs xs

makeMap :: (Ix a, Ord b) => (b -> Bool) -> Array a b -> Map.Map b [a]
makeMap p m
  = Map.fromListWith (<>)
  $ fmap (\(a, b) -> (b, [a]))
  $ filter (p . snd)
  $ Array.assocs m

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn p = groupBy ((==) `on` p)

matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x

showMatrixWith :: (a -> String) -> Array (Int, Int) a -> String
showMatrixWith s a = unlines $ fmap (join . fmap s) $ chunksOf (succ n) $ Array.elems a
  where
  (_, (n, _)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs
