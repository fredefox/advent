{-# language LambdaCase, PartialTypeSignatures #-}
{-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.Array as Array
import Data.Array (Array, Ix)
import Control.Monad
import Data.Char
import Data.Graph (Vertex)
import qualified Data.Graph as Graph
import Data.Maybe
import qualified Data.Set as Set

main :: IO ()
main = do
  m <- matrix . fmap (fmap fromChar) . lines <$> getContents
  let adj = adjacencies neighbours reachable m
  let (g, fromVertex, fromKey) = Graph.graphFromEdges adj
  let get v = fmap (\(_, key, _) -> fromMaybe undefined $ fromKey key) $ filter (\(a, _, _) -> a == v) adj
  let srcs = get 0
  let trgs = get 9
  let f = ((\(_, _, keys) -> fmap (fromMaybe undefined . fromKey) keys) . fromVertex)
  print $ length $ do
    src <- srcs
    trg <- trgs
    guard $ Graph.path g src trg
    pure $ (src, trg)
  print $ sum $ paths f <$> srcs <*> trgs

fromChar :: Char -> Int
fromChar c | isDigit c = digitToInt c
fromChar _ = -1

paths :: (Vertex -> [Vertex]) -> Vertex -> Vertex -> Int
paths outs = go Set.empty
  where
  go s i _ | Set.member i s = 0
  go _ i j | i == j = 1
  go s i j = sum $ fmap (\i' -> go (Set.insert i s) i' j) $ outs i

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (i, j) =
  [ ( pred i, j )
  , ( succ i, j )
  , ( i, pred j )
  , ( i, succ j )
  ]

reachable :: Int -> Int -> Bool
reachable x y = succ x == y

adjacencies
  :: forall ix a
  . Ix ix
  => (ix -> [ix])
  -> (a -> a -> Bool)
  -> Array ix a
  -> [(a, ix, [ix])]
adjacencies nghbs rch m = fmap go $ Array.assocs m
  where
  go :: (ix, a) -> (a, ix, [ix])
  go (ix, a) = (a, ix, ixs)
    where
    ixs :: [ix]
    ixs = foldMap step $ nghbs ix
    step ix' = case m !? ix' of
      Just b | rch a b -> pure ix'
      _ -> []

(!?) :: Ix ix => Array ix a -> ix -> Maybe a
m !? ix
  | ix `inBounds` m = Just $ m Array.! ix
  | otherwise = Nothing

inBounds :: Ix ix => ix -> Array ix a -> Bool
ix `inBounds` m = Array.bounds m `Array.inRange` ix

matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x
