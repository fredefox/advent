{-# language OverloadedStrings, TypeApplications, DerivingStrategies, LambdaCase #-}
-- {-# options_ghc -Wall #-}
module Main (main) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Function

main :: IO ()
main = do
  xs <- getContents
  let b = parse xs
  let (_, (n, m)) = Array.bounds b
  let ps = mkPs n m
  print $ solve b p
  print ps
  let ys = solve b <$> ps
  traverse_ print $ increasing ys
  print $ maximum $ ys
  where
  p = Beam (V2 (0, 0)) R
  mkPs :: Int -> Int -> [Beam Int]
  mkPs n m
    =  ((\j -> Beam (V2 (0, j)) D) <$> [0..n])
    <> ((\i -> Beam (V2 (i, 0)) R) <$> [0..m])
    <> ((\j -> Beam (V2 (n, j)) U) <$> [0..n])
    <> ((\i -> Beam (V2 (i, m)) L) <$> [0..m])

increasing :: Ord a => [a] -> [a]
increasing = increasingOn id

increasingOn :: Ord b => (a -> b) -> [a] -> [a]
increasingOn f = monotonously ((>) `on` f)

monotonously :: (a -> a -> Bool) -> [a] -> [a]
monotonously p = \case
  []     -> []
  (x:xs) -> x : monotonously p (dropWhile (p x) xs)

solve :: Array (Int, Int) Char -> Beam Int -> Int
solve b x = length $ Set.map (\(Beam (V2 p) _) -> p) $ firstDup pts
  where
  pts = iterate (step b) (Set.singleton x)

printGized :: ((Int, Int), (Int, Int)) -> Set (Int, Int) -> IO ()
printGized b = putStrLn . unlines . unMatrix . mkArray
  where
  mkArray :: Set (Int, Int) -> Array (Int, Int) Char
  mkArray s = array b (\p -> if Set.member p s then '#' else '.')

array :: Ix ix => (ix, ix) -> (ix -> e) -> Array ix e
array b f  = Array.array b (f' <$> Array.range b)
  where
  f' i = (i, f i)


unMatrix :: Array (Int, Int) Char -> [String]
unMatrix a = chunksOf (succ m) xs
  where
  xs = Array.elems a
  (_, (_, m)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

firstDup :: Eq a => [a] -> a
firstDup xs = snd $ head $ dropWhile (uncurry (/=)) $ zip xs (tail xs)

step :: Board -> Set (Beam Int) -> Set (Beam Int)
step b s = s `Set.union` Set.fromList (foldMap (stepBeam b) s)

parse :: String -> Array (Int, Int) Char
parse = matrix . lines

type Board = Array (Int, Int) Char

newtype V2 a b = V2 (a, b)

deriving stock instance (Show a, Show b) => Show (V2 a b)
instance (Num a, Num b) => Num (V2 a b) where
  V2 (a0, b0) + V2 (a1, b1) = V2 (a0 + a1, b0 + b1)
deriving newtype instance (Eq a, Eq b) => Eq (V2 a b)
deriving newtype instance (Ord a, Ord b) => Ord (V2 a b)
deriving newtype instance (Ix a, Ix b) => Ix (V2 a b)

data Dir = L | R | U | D

deriving stock instance Show Dir
deriving stock instance Eq Dir
deriving stock instance Ord Dir

data Beam a = Beam (V2 a a) Dir

deriving stock instance Show a => Show (Beam a)
deriving stock instance Eq a => Eq (Beam a)
deriving stock instance Ord a => Ord (Beam a)

(!?) :: Ix i => Array i e -> i -> Maybe e
arr !? i =
  if Array.bounds arr `Array.inRange` i
  then Just $ arr Array.! i
  else Nothing

stepBeam :: Board -> Beam Int -> [Beam Int]
stepBeam brd (Beam x d) = case brd !? xp of
  Nothing -> mempty
  Just c -> bs
    where
    ds = case c of
      '\\' -> pure $ case d of { R -> D ; L -> U ; D -> R ; U -> L }
      '/'  -> pure $ case d of { R -> U ; L -> D ; D -> L ; U -> R }
      '|'  -> case d of { R -> [D, U] ; L -> [D, U] ; _ -> pure d }
      '-'  -> case d of { U -> [L, R] ; D -> [L, R] ; _ -> pure d }
      _    -> [d]
    bs = Beam x' <$> ds
  where
  dir :: Dir -> V2 Int Int
  dir = \case
    L -> V2 (0, -1)
    R -> V2 (0, 1)
    U -> V2 (-1, 0)
    D -> V2 (1, 0)
  x'@(V2 xp) = x + dir d

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs
