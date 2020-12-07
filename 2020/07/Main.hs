{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
module Main (main, spanning, spanningWith) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree

main :: IO ()
main = do
  inp <- Text.getContents
  let bags = parse inp
  let mapping :: Map Text [(Int, Text)]
      mapping = Map.fromListWith (<>) $ foldMap (\(s, xs) -> foldMap (\(n, x) -> pure (s, [(n, x)])) xs) bags
  let invMapping :: Map Text [Text]
      invMapping = Map.fromListWith (<>) $ foldMap (\(s, xs) -> foldMap (\(_, x) -> pure (x, [s])) xs) bags
  let reachable = topsort "shiny gold bag" invMapping
  print $ Set.size $ Set.fromList reachable
  print $ solve2 "shiny gold bag" mapping

parse :: Text -> [(Text, [(Int, Text)])]
parse = fmap go . Text.lines
  where
  go :: Text -> (Text, [(Int, Text)])
  go s = case Text.splitOn "contain" s of
    [] -> (mempty, mempty)
    (x:xs) -> (norm x, foldMap parseBag (Text.splitOn "," (Text.intercalate "contain" xs)))
  parseBag :: Text -> [(Int, Text)]
  parseBag s = case reads @Int $ Text.unpack s of
    [(n, s')] -> pure (n, norm $ Text.pack s')
    _        -> mempty
  norm = Text.strip . Text.replace "bags" "bag"

topsort :: forall a . Ord a => a -> Map a [a] -> [a]
topsort = topsortWith id

topsortWith :: forall a b . Ord a => (b -> a) -> a -> Map a [b] -> [b]
topsortWith f a0 m = go a0 []
  where
  go :: a -> [b] -> [b]
  go a acc = case Map.lookup a m of
    Nothing -> acc
    Just bs -> bs <> foldr (\b bs' -> go (f b) bs') acc bs

solve2 :: forall a . Ord a => a -> Map a [(Int, a)] -> Int
solve2 a0 m = pred $ go a0
  where
  go :: a -> Int
  go a = case Map.lookup a m of
    Nothing -> 1
    Just as -> succ $ sum $ fmap (\(n, a') -> n * go a') as

spanning :: forall a . Ord a => a -> Map a [a] -> Tree a
spanning = spanningWith id

spanningWith :: forall a b . Ord a => (b -> a) -> a -> Map a [b] -> Tree a
spanningWith f a0 m = go a0
  where
  go :: a -> Tree a
  go a = Node a $ case Map.lookup a m of
    Nothing -> []
    Just as -> go . f <$> as
