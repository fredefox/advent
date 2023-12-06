{-# language OverloadedStrings, ViewPatterns, DerivingStrategies, LambdaCase #-}
-- {-# options_ghc -Wall #-}
module Main (main) where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable
import qualified Data.List as List

main :: IO ()
main = do
  (source, maps) <- parse <$> Text.getContents
  let ls = lines <$> maps
  let (Lines ls') = mconcat ls
  traverse_ print $ ls'
  print $ minimum $ run maps <$> source
  where
  lines :: [[Int]] -> Lines Int
  lines = Lines . List.sort . fmap line
  line :: [Int] -> Line Int
  line = \case
    (d:s:w:_) -> Line d s w
    _ -> error "Parse error"

run :: [[[Int]]] -> Int -> Int
run maps = f
  where
  fs = funcify <$> reverse maps
  f = foldr (.) id fs

parse :: Text -> ([Int], [[[Int]]])
parse s = case Text.splitOn "\n\n" s of
  (x:xs) -> (fmap (textRead @Int) <$> Text.words $ Text.splitOn ":" x !! 1, parseMap . Text.strip . (!! 1) . Text.splitOn ":" <$> xs)
  _ -> error "Parse error"

-- 50 98 2
-- 52 50 48
parseMap :: Text -> [[Int]]
parseMap = fmap (fmap textRead . Text.words) . Text.lines

textRead :: forall a . Read a => Text -> a
textRead = read @a . Text.unpack

funcify :: [[Int]] -> Int -> Int
funcify w n = fromMaybe n $ asum $ one n <$> w

one :: Int -> [Int] -> Maybe Int
one n w = case w of
  (r:l:k:_) | n `between` (l, l + k) -> pure $ r + (n - l)
  _ -> empty

between :: Int -> (Int, Int) -> Bool
x `between` (a, b) = a <= x && x < b

data Line a = Line a a a

instance forall a . Read a => Read (Line a) where
  readsPrec _n s = case read @a <$> words s of
    (a:b:c:_) -> pure $ (Line a b c, mempty)
    _ -> []

instance forall a . Show a => Show (Line a) where
  show (Line a b c) = show a <.> show b <.> show c
    where
    s <.> t = s <> " " <> t

-- | >>> Line 1 0 2 `intersection` Line 2 1 2
-- 3 1 1
intersection :: Ord a => Num a => Line a -> Line a -> Line a
intersection (Line d0 s0 w0) (Line d1 s1 w1) = Line d s w
  where
  s = s0 `max` s1
  r0 = s0 + w0
  r1 = s1 + w1
  r = r0 `min` r1
  w = r - s
  d = d0 + d1

-- instance (Num a, Ord a) => Semigroup (Line a) where
--   (<>) = intersection

-- instance (Num a, Ord a) => Monoid (Line a) where
--   mempty = Line 0 0 0

(\\) :: Ord a => Num a => Line a -> Line a -> Line a
Line d0 s0 w0 \\ Line d1 s1 w1 = Line d s w
  where
  d = d0
  s = s0
  r0 = s0 + w0
  r1 = s1 + w1
  r = r0 `min` r1 - s0
  w = r - s

left, right :: Ord a => Num a => Line a -> Line a -> Line a
Line d0 s0 w0 `left` Line d1 s1 w1 = case s0 `compare` s1 of
  LT -> Line d0 s0 w
    where
    r0 = s0 + w0
    r = r0 `min` s1
    w = r - s0
  EQ -> Line 0 0 0
  GT -> Line d1 s1 w
    where
    r1 = s1 + w1
    r = r1 `min` s0
    w = r - s1
Line d0 s0 w0 `right` Line d1 s1 w1 = case r0 `compare` r1 of
  LT -> Line d1 s w
    where
    s = s1 `max` r0
    w = r1 - s
  EQ -> Line 0 0 0
  GT -> Line d0 s w
    where
    s = s0 `max` r1
    w = r0 - s
  where
  r0 = s0 + w0
  r1 = s1 + w1

combine :: Ord a => Num a => Line a -> Line a -> (Line a, Line a, Line a)
combine a b = (a `left` b, a `intersection` b, a `right` b)

newtype Lines a = Lines [Line a]

deriving stock instance Show a => Show (Lines a)
-- two lines are considered "equal" if the overlap.
instance (Ord a, Num a) => Eq (Line a) where
  a == b = not $ a `disjoint` b
  -- Line d0 s0 w0 == Line d1 s1 w1 = r0 > s1 || r1 > s0
  --   where
  --   r0 = s0 + w0
  --   r1 = s1 + w1

disjoint ::  (Ord a, Num a) => Line a -> Line a -> Bool
Line d0 s0 w0 `disjoint` Line d1 s1 w1 = r0 < (s1 + 1) || r1 < (s1 + 1)
    where
    r0 = s0 + w0
    r1 = s1 + w1

instance (Ord a, Num a) => Ord (Line a) where
  l0@(Line d0 s0 w0) `compare` l1@(Line d1 s1 w1)
    | l0 == l1 = EQ
    | otherwise = s0 `compare` s1

merge :: Ord a => Num a => [Line a] -> [Line a] -> [Line a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xss) ys@(y:yss) = case x `compare` y of
  LT -> x : merge xss ys
  EQ -> x `left` y : x `intersection` y : x `right` y : merge xss yss
  GT -> y : merge xs yss

instance (Ord a, Num a) => Semigroup (Lines a) where
  Lines xs <> Lines ys = Lines $ merge xs ys

instance (Ord a, Num a) => Monoid (Lines a) where
  mempty = Lines mempty
