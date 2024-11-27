#!/usr/bin/env runhaskell
{-# language LambdaCase, ViewPatterns #-}
module Main (main, matrix, unMatrix) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import Data.Foldable
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ix as Ix
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  xs <- getContents
  let m = matrix $ lines xs
  print $ sum $ solve m

solve :: Array (Int, Int) Char -> [Int]
solve m = numbersAt m $ filter (near m) $ beginnings m

numbersAt :: Array (Int, Int) Char -> [(Int, Int)] -> [Int]
numbersAt m = fmap (step 0)
  where
  step :: Int -> (Int, Int) -> Int
  step acc ix@(i, j)
    | p         = step (acc * 10 + (Char.digitToInt c)) (i, succ j)
    | otherwise = acc
    where
    p = (Array.bounds m `Ix.inRange` ix) && Char.isDigit c
    c = m Array.! ix

beginnings :: (Num b, Ix a, Ix b, Enum b) => Array (a, b) Char -> [(a, b)]
beginnings m = reverse $ go [] $ Array.assocs m
  where
  go acc ((ix@(i, j), c):xs)
    | isBeginningOfNumber
    = go (ix : acc) xs
    where
    isBeginningOfNumber
      =  Char.isDigit c
      && (j == 0 || not (Char.isDigit (m Array.! (i, pred j))))
  go acc (_:xs) = go acc xs
  go acc [] = acc

nums :: (Ix a, Ix b, Enum b) => Array (a, b) Char -> (a, b) -> Int
nums m = read @Int . reverse . go [] 
  where
  go acc ix@(i, j) = case safeIndex m ix of
    Nothing -> acc
    Just e -> if Char.isDigit e then go (e:acc) (i, succ j) else acc

near :: Array (Int, Int) Char -> (Int, Int) -> Bool
near m ((i, j))
  =  symb (pred i)
  || symb i
  || symb (succ i)
  || next
  where
  symb i' = fromMaybe False $ isSymbol <$> safeIndex m (i', pred j)
  (_, jmax) = snd $ Array.bounds m
  next = (j <= jmax) && (Char.isDigit (m Array.! (i, j)) || Char.isDigit (m Array.! (i, pred j))) && near m (i, succ j)

safeIndex :: Ix i => Array i a -> i -> Maybe a
safeIndex m ix = if Array.bounds m `Ix.inRange` ix then Just $ m Array.! ix else Nothing

isSymbol :: Char -> Bool
isSymbol c | Char.isDigit c = False
isSymbol c | Char.isSpace c = False
isSymbol '.' = False
isSymbol _ = True

matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x

unMatrix :: Array (Int, Int) a -> [[a]]
unMatrix arr = chunksOf n $ Array.elems arr
  where
  n = succ $ fst $ snd $ Array.bounds arr

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go
  where
  go [] = []
  go xs = case splitAt n xs of
    (a, b) -> a : go b

array :: Ix ix => (ix, ix) -> (ix -> e) -> Array ix e
array b f  = Array.array b (f' <$> Array.range b)
  where
  f' i = (i, f i)

