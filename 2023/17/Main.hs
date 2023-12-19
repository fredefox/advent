module Main (main) where

import Data.Array.IO as Array
import Data.Array as Array
import Control.Monad
import qualified Data.Ix as Ix

main :: IO ()
main = do
  w <- parse <$> getContents
  m <- Array.newArray @IOArray @(Maybe Int) @IO (Array.bounds w) Nothing
  step w m 0 (0, 0)
  e <- Array.getAssocs m
  print e

step :: Array (Int, Int) Int -> IOArray (Int, Int) (Maybe Int) -> Int -> (Int, Int) -> IO ()
step w a = go
  where
  go :: Int -> (Int, Int) -> IO ()
  go v ix = do
    b <- Array.getBounds a
    let ixs = filter (b `Ix.inRange`) $ neighbors ix
    ixs `forM_` \ix' -> do
      e <- Array.readArray a ix'
      let d = w Array.! ix'
      let v' = v + d
      let vmin' = case e of { Nothing -> v' ; Just vmin -> vmin `min` v' }
      Array.writeArray a ix' (Just vmin')
      when (v' <= vmin') (go v' ix')

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
