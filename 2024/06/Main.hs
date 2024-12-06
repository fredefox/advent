-- I haven't done part 2, but my idea for solving it is writing an
-- algorithm to determine if the guard is in a loop with the current
-- configuration and then for each position where we can put an
-- additional obstacle, check if that configuration means there is a
-- loop and then count them all.  I envisage that this solution will
-- have time complexity O(n ^ 2) where n is the size of the maze
-- (number of cells).

{-# language GHC2021, LambdaCase, DerivingStrategies #-}
module Main (main) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import qualified Data.Set as Set

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = matrix xs
  indicesOf ('^' ==) m `forM_` part1 m

part1 :: Array (Int, Int) Char -> (Int, Int) -> IO ()
part1 m idx = do
  let posAndDir = fmap (\(a, b) -> (a, b)) $ iterate (step heading m) (North, idx)
  let coverages = scanl (flip Set.insert) mempty posAndDir
  let s = fst $ head $ dropWhile (uncurry (/=)) $ zip coverages (tail coverages)
  print $ Set.size $ Set.map snd s

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

deriving stock instance Show (Direction a)
deriving stock instance Eq (Direction a)
deriving stock instance Ord (Direction a)

type Heading a = Direction a -> a -> a

heading :: Heading (Int, Int)
heading = \case
  North -> \(i, j) -> (pred i, j)
  South -> \(i, j) -> (succ i, j)
  East -> \(i, j) -> (i, succ j)
  West -> \(i, j) -> (i, pred j)

step
  :: Ix ix
  => Heading ix
  -> Array ix Char
  -> (Direction ix, ix)
  -> (Direction ix, ix)
step heading m (d, ix) = case check heading m (d, ix) of
  Just d' -> (d', heading d' ix)
  Nothing -> (d, ix)

check
  :: Ix ix
  => Heading ix
  -> Array ix Char
  -> (Direction ix, ix)
  -> Maybe (Direction ix)
check heading m (d, ix) = case m !? heading d ix of
  Nothing -> Nothing
  Just '#' -> check heading m (turn d, ix)
  Just _ -> Just d

turn :: Direction ix -> Direction ix
turn = \case
  North -> East
  East -> South
  South -> West
  West -> North

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
