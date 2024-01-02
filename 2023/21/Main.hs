{-# language MultiWayIf, LambdaCase, TypeApplications, DerivingStrategies #-}
module Main (main) where

import Data.Foldable
import Data.Char
import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Array.MArray as MArray
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray
import Control.Monad
import Data.Ix
import Data.Graph (Graph, Vertex, Bounds)
import qualified Data.Graph as Graph
import Data.Maybe
import Control.Applicative
import qualified Data.Tree as Tree
import Data.Tree (Tree, Forest)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (sort)
import Control.Monad.State (State)
import qualified Control.Monad.State as State

main :: IO ()
main = do
  a <- parse <$> getContents
  -- print a
  let (g, node, vertex) = graph (/= '#') ns a
  -- print g
  let ((s, _):_) = filter ((== 'S') . snd) $ Array.assocs a
  -- print s
  let Just sVertex = vertex s
  -- print sVertex
  -- print $ filter (\(c, _, _) -> c == 'S') $ node <$> Graph.vertices g
  -- print $ sort $ solve (>= 0) g k [(sVertex, k)]
  print $ length $ filter ((== 0) . (`mod` 2)) $ solve (>= 0) g [(sVertex, k)]
  -- putStrLn . Tree.drawTree $ fmap show t
  -- print $ solve k t
  where
  ns (i, j) =
    [ pred i |> j
    , succ i |> j
    , i |> pred j
    , i |> succ j
    ]
  (|>) = (,)
  k = 64

newtype SetM s a = SetM { runSetM :: State ([(Vertex, Int)], IntSet) a }

deriving newtype instance Functor (SetM s)
deriving newtype instance Applicative (SetM s)
deriving newtype instance Monad (SetM s)

run          :: [(Vertex, Int)] -> Bounds -> SetM s a -> a
run vs _ act     = fst $ State.runState (runSetM act) (vs, mempty)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ State.state $ \(vs, m) -> (IntSet.member v m, (vs, m))

include      :: Vertex -> SetM s ()
include v     = SetM $ State.state $ \(vs, m) -> ((), (vs, IntSet.insert v m))

enqueue :: (Vertex, Int) -> SetM s ()
enqueue v = SetM $ State.state $ \(vs, m) -> ((), (v : vs, m))

dequeue :: [a] -> Maybe (a, [a])
dequeue = \case
  [] -> Nothing
  xs -> Just (last xs, init xs)

pop :: SetM s (Maybe (Vertex, Int))
pop = SetM $ State.state $ \(vs, m) -> case dequeue vs of
  Nothing -> (Nothing, (vs, m))
  Just (v, vs') -> (Just v, (vs', m))

push :: (Vertex, Int) -> SetM s ()
push v = SetM $ State.modify $ \(vs, m) -> (v:vs, m)

solve :: (Int -> Bool) -> Graph -> [(Vertex, Int)] -> [Vertex]
solve p g vs0 = run vs0 (Array.bounds g) $ go
  where
  go :: SetM s [Vertex]
  go = do
    mv <- pop
    case mv of
      Nothing -> pure mempty
      Just (v, n) -> do
        visited <- contains v
        if
          | visited -> go
          -- TODO: the same vertex may appear in `g Array.! v` and `vs`.
          -- Suggested fix: You could try and take inspiration from
          -- https://hackage.haskell.org/package/containers-0.7/docs/src/Data.Graph.html#dfs
          | p n -> do
              include v
              traverse_ (\v' -> push (v', pred n)) (g Array.! v)
              (n:) <$> go
              -- v : (go' (pred n) (g Array.! v) <> go' n vs)
          | otherwise -> go

parse :: String -> Array (Int, Int) Char
parse = matrix . lines

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs

graph
  :: forall i e
  . Ix i
  => (e -> Bool)
  -> (i -> [i])
  -> Array i e
  -> (Graph, Vertex -> (e, i, [i]), i -> Maybe Vertex)
graph p neighbors a = Graph.graphFromEdges xs
  where
  xs :: [(e, i, [i])]
  xs = foldMap f $ Array.assocs a
  f :: (i, e) -> [(e, i, [i])]
  f (i, e) | p e = pure (e, i, ys)
    where
    ys :: [i]
    ys = foldMap g $ neighbors i
    g :: i -> [i]
    g j | Array.bounds a `inRange` j && p (a Array.! j) = pure j
        | otherwise = empty
  f _ | otherwise = empty
