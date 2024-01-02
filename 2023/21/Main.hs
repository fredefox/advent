{-# language MultiWayIf, LambdaCase, TypeApplications, DerivingStrategies #-}
-- {-# options_ghc -Wall #-}
module Main (main) where

import Data.Foldable
import Data.Array (Array)
import qualified Data.Array as Array
import Control.Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  a <- parse <$> getContents
  let ((s, _):_) = filter ((== 'S') . snd) $ Array.assocs a
  print $ length $ filter ((== 0) . (`mod` 2)) $ snd <$> solve (>= 0) pred (reachable a) [(s, k)]
  where
  k :: Int
  k = 64

contains :: Ord e => e -> State (a, Set e) Bool
contains v = (Set.member v) . snd <$> State.get

include :: Ord e => e -> State (a, Set e) ()
include v = State.modify $ \(vs, m) -> (vs, Set.insert v m)

dequeue :: [a] -> Maybe (a, [a])
dequeue = \case
  [] -> Nothing
  xs -> Just (last xs, init xs)

pop :: State ([e], s) (Maybe e)
pop = State.state $ \(vs, m) -> case dequeue vs of
  Nothing -> (Nothing, (vs, m))
  Just (v, vs') -> (Just v, (vs', m))

push :: e -> State ([e], s) ()
push v = State.modify $ \(vs, m) -> (v : vs, m)

solve
  :: forall n i
  . Ord i
  => (n -> Bool)
  -> (n -> n)
  -> (i -> [i])
  -> [(i, n)]
  -> [(i, n)]
solve p nx rchbl vs0 = State.evalState go (vs0, mempty)
  where
  go :: State ([(i, n)], Set i) [(i, n)]
  go = do
    mv <- pop
    case mv of
      Nothing -> pure mempty
      Just (v, n) -> do
        visited <- contains v
        if
          | visited -> go
          | p n -> do
              include v
              traverse_ (\v' -> push (v', nx n)) (rchbl v)
              ((v, n):) <$> go
          | otherwise -> go

reachable :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
reachable a = foldMap go . ns
  where
  ns (i, j) =
    [ pred i |> j
    , succ i |> j
    , i |> pred j
    , i |> succ j
    ]
  (|>) = (,)
  (_, (n, m)) = Array.bounds a
  go :: (Int, Int) -> [(Int, Int)]
  go (i, j) = if a Array.! ix /= '#' then pure ix else mempty
    where
    ix = (i `mod` n, j `mod` m)

parse :: String -> Array (Int, Int) Char
parse = matrix . lines

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs
