{-# language OverloadedStrings, ViewPatterns, LambdaCase #-}
module Main (main) where

import Data.Foldable
import qualified Data.List as List
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Maybe
import Control.Applicative

main :: IO ()
main = do
  xs <- fmap Text.unpack . Text.splitOn "\n\n" <$> Text.getContents
  print $ sum $ solve <$> xs

solve :: String -> Int
solve p = case go ls <|> (*100) <$> (go rs) of
  [] -> error "No reflection found"
  (x:_) -> x
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
