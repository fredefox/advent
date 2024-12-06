{-# language GHC2021, LambdaCase, DerivingStrategies #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  xs <- lines <$> getContents
  let m = matrix xs
  indicesOf ('^' ==) m `forM_` \idx -> do
    let w = walk heading m idx
    print $ areaOfWalk w
    print $ solve heading m idx (Array.indices m)

solve :: Heading (Int, Int) -> Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)] -> Int
solve hdn m idx0 = foldl go 0
  where
  go :: Int -> (Int, Int) -> Int
  go n idx = if repitition w' then succ n else n
    where
    m' = m Array.// [(idx, '#')]
    w' = walk hdn m' idx0

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

type Walk ix = [(Direction ix, ix)]

walk :: Ix ix => Heading ix -> Array ix Char -> ix -> Walk ix
walk hdn m idx = iterateMaybe (step hdn m) (North, idx)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : case f a of
  Nothing -> []
  Just b -> iterateMaybe f b

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
step hdn m (d, ix) = f <$> check hdn m (d, ix)
  where
  f d' = (d', hdn d' ix)

check
  :: forall ix
  . Ix ix
  => Heading ix
  -> Array ix Char
  -> (Direction ix, ix)
  -> Maybe (Direction ix)
check hdn m = go 0
  where
  go :: Int -> (Direction ix, ix) -> Maybe (Direction ix)
  go n (d, ix) = case m !? hdn d ix of
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
