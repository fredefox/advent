{-# LANGUAGE GHC2021, PartialTypeSignatures, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec hiding (parse)
import Data.Functor.Identity
import Data.Graph
import qualified Data.Map as Map
import Data.List ( (\\) )

fromEdges :: forall a . Ord a => [(a, a)] -> Map.Map a [a]
fromEdges xs = Map.fromListWith (<>) (xs0 <> xs1)
  where
  xs0 = (\(a, b) -> (a, [b])) <$> xs
  xs1 = fmap (\(_, a) -> (a, [])) xs

type Adj node key = [(node, key, [key])]

fromMap
  :: forall key node
  . Ord key
  => (key -> node)
  -> Map.Map key [key]
  -> Adj node key
fromMap f m = fmap step $ Map.toList m
  where
  step :: (key, [key]) -> (node, key, [key])
  step (x, xs) = (f x, x, xs)

buildGraph :: Ord key => (key -> node) -> [(key, key)] -> Adj node key
buildGraph f = fromMap f . fromEdges

buildRules :: [[Int]] -> [Int] -> [(Int, Int)]
buildRules xs ys = relevantRules xs ys <> zip ys (tail ys)

relevantRules :: [[Int]] -> [Int] -> [(Int, Int)]
relevantRules xs ys = fmap go xs0
  where
  xs0 :: [[Int]]
  xs0 = filter (null . (\\ ys)) xs
  go (a:b:_) = (a, b)
  go _ = error "Invalid input"

main :: IO ()
main = do
  Right (rs, pgs) <- parse <$> getContents
  print $ solve rs pgs
  let wrong = filter (not . valid rs) pgs
  print $ sum $ median <$> tops rs <$> wrong

tops :: [[Int]] -> [Int] -> [Int]
tops rs pg = go . f <$> topSort g
  where
  (g, f, _) = graphFromEdges $ buildGraph id $ relevantRules rs pg
  go (a, _, _) = a

solve :: [[Int]] -> [[Int]] -> Int
solve rs pgs = sum $ fmap median $ filter (valid rs) pgs

valid :: [[Int]] -> [Int] -> Bool
valid rs pg = isAcyclic $ buildGraph id $ buildRules rs pg

parse :: String -> Either ParseError ([[Int]], [[Int]])
parse = runParser parser () mempty

median :: [a] -> a
median xs = xs !! (n `div` 2)
  where
  n = length xs
  
isAcyclic :: Ord key => Adj node key -> Bool
isAcyclic = all go . stronglyConnComp
  where
  go :: SCC node -> Bool
  go = \case
    AcyclicSCC{} -> True
    _ -> False

parser :: Parsec String () ([[Int]], [[Int]])
parser = do
  rs <- many rule
  _ <- newline
  ps <- many page
  pure (rs, ps)

page :: ParsecT String () Identity [Int]
page
  = number `sepBy` char ','
  <* newline

rule :: ParsecT String u Identity [Int]
rule
  = number `sepBy1` char '|'
  <* newline

number :: Stream s Identity Char => Parsec s u Int
number = read <$> many1 digit
