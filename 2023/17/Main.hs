{-# language MultiWayIf #-}
module Main (main) where

import Data.Array.IO as Array
import Data.Array as Array
import Control.Monad
import qualified Data.Ix as Ix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as Queue

main :: IO ()
main = do
  w <- parse <$> getContents
  let b@(_, ixmax) = Array.bounds w
  m <- Array.newArray @IOArray @(Map (Int, Int) Int) @IO b mempty
  step w m (Queue.singleton ((0, 0), (0, 0)) 0)
  ascs <- Array.getAssocs m
  e <- Array.readArray m ixmax
  print $ foldMap Min e

instance (Ord k, Ord p) => Semigroup (PSQ k p) where
  a <> b = Queue.foldr (\(k :-> v) -> Queue.insert k v) a b

step
  :: Array (Int, Int) Int
  -> IOArray (Int, Int) (Map (Int, Int) Int)
  -> PSQ ((Int, Int), (Int, Int)) Int
  -> IO ()
step w a = go
  where
  go
    :: PSQ ((Int, Int), (Int, Int)) Int
    -> IO ()
  go q = case Queue.minView q of
    Nothing -> pure ()
    Just ((ix, dir) :-> v, q') -> do
      b <- Array.getBounds a
      qs <- deltas dir `forM` \delta -> do
        let ix' = delta `plus` ix
        let dir' = dir `plus'` delta
        if not (b `Ix.inRange` ix' && lim dir')
        then pure []
        else do
          e <- Array.readArray a ix'
          let d = w Array.! ix'
          let v' = v + d
          let vmin' = case Map.lookup dir' e of { Nothing -> v' ; Just vmin -> vmin `min` v' }
          Array.writeArray a ix' (Map.insert dir' vmin' e)
          if (v' <= vmin') then pure [(ix', dir') :-> v'] else pure []
      let q'' = q' <> Queue.fromList (join qs)
      let ixmax = snd b
      when (ix /= ixmax) $ go q''

plus' :: (Int, Int) -> (Int, Int) -> (Int, Int)
(_, j) `plus'` (0, j') = (0, j + j')
(i, _) `plus'` (i', _) = (i + i', 0)

lim (i, j) = abs i <= 10 && abs j <= 10 && (i /= 0 || j /= 0)

(a0, b0) `plus` (a1, b1) = (a0 + a1, b0 + b1)

deltas :: (Int, Int) -> [(Int, Int)]
deltas (i, j) = if
  | i == 0 && j < 0 && abs j < 4 -> [(0, -1)]
  | i == 0 && j > 0 && abs j < 4 -> [(0, 1)]
  | j == 0 && i < 0 && abs i < 4 -> [(-1, 0)]
  | j == 0 && i > 0 && abs i < 4 -> [(1, 0)]
  | otherwise -> [(1, 0), (-1, 0), (0, 1), (0, -1)]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) =
  [ (succ i, j)
  , (pred i, j)
  , (i, succ j)
  , (i, pred j)
  ]

parse :: String -> Array (Int, Int) Int
parse = matrix . fmap (fmap (read . pure)) . lines

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs
