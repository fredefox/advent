{-# options_ghc -Wall #-}
module Main (main) where

import Data.Array (Array)
import qualified Data.Array as Array
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Ix as Ix

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = matrix xs
  let n = succ (solve m) `div` 2
  print n

solve :: Array (Int, Int) Char -> Int
solve m = case filter (\(_, x) -> x == 'S') asc of
  ((start,_):_) -> go start (mempty :: Set (Int, Int))
  _ -> error "No start node"
  where
  asc = Array.assocs m
  b = Array.bounds m
  go :: (Int, Int) -> Set (Int, Int) -> Int
  go idx@(i, j) s
    | not $ b `Ix.inRange` idx = -1
    | idx `elem` s = 0
    | otherwise = succ $ case m Array.! idx of
      '|' -> go (pred i, j) s' `max` go (succ i, j) s'
      '-' -> go (i, pred j) s' `max` go (i, succ j) s'
      'L' -> go (pred i, j) s' `max` go (i, succ j) s'
      'J' -> go (pred i, j) s' `max` go (i, pred j) s'
      '7' -> go (succ i, j) s' `max` go (i, pred j) s'
      'F' -> go (succ i, j) s' `max` go (i, succ j) s' 
      '.' -> -1
      'S' ->
                   go (succ i, j) s'
             `max` go (i, succ j) s'
             `max` go (pred i, j) s'
             `max` go (i, pred j) s' 
      _   -> error "Parse error"
      where
      s' = Set.insert idx s


matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs
