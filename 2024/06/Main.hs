-- I haven't done part 2, but my idea for solving it is writing an
-- algorithm to determine if the guard is in a loop with the current
-- configuration and then for each position where we can put an
-- additional obstacle, check if that configuration means there is a
-- loop and then count them all.  I envisage that this solution will
-- have time complexity O(n ^ 2) where n is the size of the maze
-- (number of cells).

{-# language GHC2021, LambdaCase #-}
module Main (main) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List (isPrefixOf)
import Data.Foldable
import qualified Data.Set as Set

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = matrix xs
  let (idx:_) = indicesOf ('^' ==) m
  part1 m idx

part1 :: Array (Int, Int) Char -> (Int, Int) -> IO ()
part1 m idx = do
  let positions = fmap snd $ iterate (step m) (directions, idx)
  let coverages = fmap Set.size $ scanl (flip Set.insert) mempty positions
  let (_, (t, u)) = Array.bounds m
  -- The drop thing is a weird way to determine if we're done.  That's
  -- because I'm not saving the directions we're heading, so it's not
  -- sufficient to check that there are no new positions added at a
  -- given point in time. 4 * N * M is an upper bound for how many
  -- steps we can walk around in the maze before we've been in the
  -- same position in the same direction twice (and therefore are in a
  -- loop).
  print $ head $ drop (4 * t * u) $ coverages

update :: Array (Int, Int) Char -> [(Int, Int)] -> [Array (Int, Int) Char]
update m (ix:ixs) = m' : update m ixs
  where
  m' = m Array.// [(ix, 'X')]

showMatrix :: (a -> String) -> Array (Int, Int) a -> String
showMatrix shw a = unlines $ fmap (join . fmap shw) $ chunksOf (succ n) $ Array.elems a
  where
  (_, (n, _)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

data Direction a = North | South | East | West

class Heading a where
  heading :: Direction a -> a -> a

instance Heading (Int, Int) where
  heading :: Direction (Int, Int) -> (Int, Int) -> (Int, Int)
  heading = \case
    North -> \(i, j) -> (pred i, j)
    South -> \(i, j) -> (succ i, j)
    East -> \(i, j) -> (i, succ j)
    West -> \(i, j) -> (i, pred j)

directions :: [Direction a]
directions = cycle [North, East, South, West]

step
  :: Ix ix
  => Heading ix
  => Array ix Char
  -> ([Direction ix], ix)
  -> ([Direction ix], ix)
step m (ds, ix) = case check m (ds, ix) of
  ds'@(d:_) -> (ds', heading d ix)
  _ -> ([], ix)

check
  :: Heading ix
  => Ix ix
  => Array ix Char
  -> ([Direction ix], ix)
  -> [Direction ix]
check m (d:ds, ix) = case m !? heading d ix of
  Nothing -> []
  Just '#' -> check m (ds, ix)
  Just _ -> d:ds
check _ ([], _) = []

(!?) :: Ix i => Array i e -> i -> Maybe e
arr !? i =
  if Array.bounds arr `Array.inRange` i
  then Just $ arr Array.! i
  else Nothing

indicesOf :: forall ix a . Ix ix => (a -> Bool) -> Array ix a -> [ix]
indicesOf p m = foldMap go $ Array.assocs m
  where
  go :: (ix, a) -> [ix]
  go (ix, c)
    | p c = pure ix
    | otherwise = mempty
  
matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x
