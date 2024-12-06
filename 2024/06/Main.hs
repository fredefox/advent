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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import System.IO

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = matrix xs
  indicesOf ('^' ==) m `forM_` \idx -> do
    let w = walk heading m idx
    print $ areaOfWalk w
    let ns = scanl @Int (go heading m idx) 0 (Array.indices m)
    print $ last ns

go :: Heading (Int, Int) -> Array (Int, Int) Char -> (Int, Int) -> Int -> (Int, Int) -> Int
go heading m idx0 n idx = if repitition w' then succ n else n
  where
  m' = m Array.// [(idx, '#')]
  w' = walk heading m' idx0

areaOfWalk :: Ord a => Walk a -> Int
areaOfWalk w = Set.size $ Set.map snd $ collect w

-- | `collect xs` contains the prefix of `xs` up until the first
-- repitition.
collect :: forall a . Ord a => [a] -> Set a
collect = go mempty
  where
  go :: Set a -> [a] -> Set a
  go acc = \case
    [] -> acc
    (x:xs)
      | x `Set.member` acc -> acc
      | otherwise -> go (Set.insert x acc) xs

-- | Similar to @collect@ but just determines if there is a repition.
repitition :: forall a . Ord a => [a] -> Bool
repitition = go mempty
  where
  go :: Set a -> [a] -> Bool
  go acc = \case
    [] -> False
    (x:xs)
      | x `Set.member` acc -> True
      | otherwise -> go (Set.insert x acc) xs

-- | Returns the set of all position-direction pairs that have been
-- visited in this run.
visitedPositions :: Ix ix => Heading ix -> Array ix Char -> ix -> (Set.Set (Direction ix, ix))
visitedPositions heading m idx = fst $ head $ dropWhile (uncurry (/=)) $ zip coverages (tail coverages)
  where
  coverages = scanl (flip Set.insert) mempty $ walk heading m idx

type Walk ix = [(Direction ix, ix)]

walk :: Ix ix => Heading ix -> Array ix Char -> ix -> Walk ix
walk heading m idx = iterateMaybe (step heading m) (North, idx)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : case f a of
  Nothing -> []
  Just b -> iterateMaybe f b

-- hasLoop :: forall ix . Ix ix => Heading ix -> Array ix Char -> ix -> Bool
-- hasLoop heading m ix = go mempty (uniquePrefix $ walk heading m ix)
--   where
--   go :: Set.Set (Direction ix, ix) -> Walk ix -> Bool
--   go _ [] = False
--   go s (x:xs)
--     | Set.member x s = True
--     | otherwise    = go (Set.insert x s) xs

-- uniquePrefix :: Eq a => [a] -> [a]
-- uniquePrefix [] = []
-- uniquePrefix xs = fmap snd $ takeWhile (uncurry (/=)) $ zip xs (tail xs)

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
  -> Maybe (Direction ix, ix)
step heading m (d, ix) = f <$> check heading m (d, ix)
  where
  f d' = (d', heading d' ix)

check
  :: Ix ix
  => Heading ix
  -> Array ix Char
  -> (Direction ix, ix)
  -> Maybe (Direction ix)
check heading m = go 0
  where
  go n (d, ix) = case m !? heading d ix of
    _ | n == 4 -> Nothing
    Nothing -> Nothing
    Just '#' -> go (succ n) (turn d, ix)
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
