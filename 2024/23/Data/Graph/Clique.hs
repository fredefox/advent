{-# options_ghc -Wall #-}
module Data.Graph.Clique (cliques) where

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array as Array

cliques :: Graph -> [Set Vertex]
cliques g = bronKerbosch g mempty (Set.fromList (Graph.vertices g)) mempty

bronKerbosch :: Graph -> Set Vertex -> Set Vertex -> Set Vertex -> [Set Vertex]
bronKerbosch g r p x
  | Set.null p && Set.null x = pure r
  | otherwise = case foldr go (mempty, p, x) p of
      (c, _, _) -> c
  where
  go
    :: Vertex
    -> ([Set Vertex], Set Vertex, Set Vertex)
    -> ([Set Vertex], Set Vertex, Set Vertex)
  go v (acc, p', x') = (cs <> acc, Set.delete v p', Set.insert v x')
    where
    cs :: [Set Vertex]
    cs = bronKerbosch g
      (Set.insert v r)
      (nv `Set.intersection` p')
      (nv `Set.intersection` x')
    nv :: Set Vertex
    nv = Set.fromList $ g Array.! v
