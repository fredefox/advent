module Main (main) where

import Data.Array.IO as Array
import Data.Array as Array
import Control.Monad
import qualified Data.Ix as Ix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup

main :: IO ()
main = do
  w <- parse <$> getContents
  let b@(_, ixmax) = Array.bounds w
  m <- Array.newArray @IOArray @(Map (Int, Int) Int) @IO b mempty
  step w m 0 (0, 0) (0, 0)
  ascs <- Array.getAssocs m
  -- print ascs
  e <- Array.readArray m ixmax
  print $ foldMap Min e

step :: Array (Int, Int) Int -> IOArray (Int, Int) (Map (Int, Int) Int) -> Int -> (Int, Int) -> (Int, Int) -> IO ()
step w a = go
  where
  go :: Int -> (Int, Int) -> (Int, Int) -> IO ()
  go v dir ix = do
    b <- Array.getBounds a
    -- let ixs = filter (b `Ix.inRange`) $ neighbors ix
    deltas `forM_` \delta -> do
      let ix' = delta `plus` ix
      let dir' = dir `plus'` delta
      when (b `Ix.inRange` ix' && lim dir') $ do
        -- print ix'
        -- print dir'
        e <- Array.readArray a ix'
        let d = w Array.! ix'
        let v' = v + d
        let vmin' = case Map.lookup dir' e of { Nothing -> v' ; Just vmin -> vmin `min` v' }
        Array.writeArray a ix' (Map.insert dir' vmin' e)
        when (v' <= vmin') (go v' dir' ix')

plus' :: (Int, Int) -> (Int, Int) -> (Int, Int)
(_, j) `plus'` (0, j') = (0, j + j')
(i, _) `plus'` (i', _) = (i + i', 0)

lim (i, j) = abs i <= 3 && abs j <= 3

(a0, b0) `plus` (a1, b1) = (a0 + a1, b0 + b1)

deltas :: [(Int, Int)]
deltas = [(1, 0), (-1, 0), (0, 1), (0, -1)]

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
