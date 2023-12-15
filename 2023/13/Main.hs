{-# language OverloadedStrings, ViewPatterns, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.List as List
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Applicative

fromList :: a -> [a] -> a
fromList d = \case
  [] -> d
  (x:_) -> x

main :: IO ()
main = do
  xs <- fmap Text.unpack . Text.splitOn "\n\n" <$> Text.getContents
  print $ sum $ (fromList undefined . solve) <$> xs
  print $ sum $ (fromList undefined . solve2) <$> xs

-- | Find all new reflection lines arising from a smudge anywhere.
solve2 :: String -> [Int]
solve2 x = foldMap go $ replacements x
  where
  go r = solve r List.\\ solve x

replacements :: String -> [String]
replacements = \case
  [] -> [[]]
  (x:xs) -> a <|> b
    where
    a = do
      x' <- smudge x
      pure $ x':xs
    b = (x:) <$> replacements xs

smudge :: Char -> String
smudge = \case
  '.' -> pure '#'
  '#' -> pure '.'
  _ -> empty

solve :: String -> [Int]
solve p = go ls <|> (*100) <$> (go rs)
  where
  ls = lines p
  rs = List.transpose ls
  go = Set.toList . intersections . fmap reflections

intersections :: Foldable f => f IntSet -> IntSet
intersections = foldl1 Set.intersection

reflections :: String -> IntSet
reflections = Set.fromList . fmap fst . filter go . init . tail . zip [0..] . splits
  where
  reflection :: String -> String -> Bool
  reflection a b = and $ zipWith (==) (reverse a) b
  go (_n, (a, b)) = a `reflection` b

splits :: [a] -> [([a], [a])]
splits xs = List.inits xs `zip` List.tails xs
