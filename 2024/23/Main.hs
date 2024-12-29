{-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec hiding (many, optional)
import Control.Exception hiding (try)
import Data.Char
import Control.Applicative
import qualified Data.Graph as Graph
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable
import qualified Data.Graph.Clique as Clique
import System.Environment

main :: IO ()
main = do
  inp <- getContents
  args <- getArgs
  case runParser parser () mempty inp of
    Left e -> throwIO e
    Right groups -> do
      let m = buildMaps groups
      let adj = fmap (\(x, xs) -> (x, x, xs)) $ Map.toList m
      let (g, fromV, _fromK) = Graph.graphFromEdges @String @String adj
      let showV = ((\(idf, _, _) -> idf) . fromV)
      let printClique = putStrLn . unwords . fmap showV
      if "pt1" `elem` args
      then
        traverse_ printClique
          $ List.nub
          $ foldMap (groupsOf 3)
          $ fmap Set.toList
          $ filter ((>= 3) . length)
          $ Clique.cliques g
      else traverse_ (printClique . Set.toList) $ Clique.cliques g

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = filter ((== n) . length) . List.subsequences

buildMaps :: Ord a => [[a]] -> Map a [a]
buildMaps = Map.unionsWith (<>) . fmap buildMap

buildMap :: Ord a => [a] -> Map a [a]
buildMap = Map.fromListWith (<>) . unconses

unconses :: [a] -> [(a, [a])]
unconses xs0 = foldMap step $ init $ zip (List.inits xs0) (List.tails xs0)
  where
  step (xs, x:ys) = pure (x, xs <> ys)
  step _ = empty

parser :: Parsec String () [[String]]
parser = many $ line <* newline
  where
  line = ident `sepBy` satisfy (`elem` "- ")
  ident = many (satisfy isAlpha)

