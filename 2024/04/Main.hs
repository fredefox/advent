{-# language GHC2021, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List (isPrefixOf)

main :: IO ()
main = do
  contents <- lines <$> getContents
  let m = matrix contents
  print $ sum $ fmap (\ix -> checkDirections ix m) $ Array.indices m
  let ixs = filter (\ix -> check3x3 ix m) (Array.indices m)
  print $ length ixs

check3x3 :: (Int, Int) -> Array (Int, Int) Char -> Bool
check3x3 ix@(a, b) m = checkSquare ix (a + 2, b + 2) m

square :: Ix ix => ix -> ix -> Array ix a -> [a]
square a b m = lkps (Array.range (a, b))
  where
  lkps = catMaybes . fmap (m !?)

zipWith' :: c -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' d f (x:xs) (y:ys) = f x y : zipWith' d f xs ys
zipWith' d f (_:xs) [] = d : zipWith' d f xs []
zipWith' d f [] (_:ys) = d : zipWith' d f [] ys
zipWith' _ _ [] [] = []

checkSquare :: Ix ix => ix -> ix -> Array ix Char -> Bool
checkSquare a b m = any f targets
  where
  f = \target -> and $ zipWith' False match target (square a b m)
  targets = join <$>
    [ [ "M.S"
      , ".A."
      , "M.S"
      ]
    , [ "M.M"
      , ".A."
      , "S.S"
      ]
    , [ "S.M"
      , ".A."
      , "S.M"
      ]
    , [ "S.S"
      , ".A."
      , "M.M"
      ]
    ]
  match :: Char -> Char -> Bool
  match '.' _ = True
  match c0 c1 = c0 == c1

checkDirections :: (Int, Int) -> Array (Int, Int) Char -> Int
checkDirections ix m = length $ filter (\d -> checkDir d ix m) directions

checkDir :: Ix ix => (ix -> ix) -> ix -> Array ix Char -> Bool
checkDir d ix m = ("XMAS" `isPrefixOf`) $ line d ix m

matrix :: [[a]] -> Array (Int, Int) a
matrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x

inBounds :: Ix a => Array a e -> a -> Bool
m `inBounds` ix = Array.bounds m `Array.inRange` ix

(!?) :: Ix i => Alternative f => Array i a -> i -> f a
m !? ix
  | m `inBounds` ix = pure $ m Array.! ix
  | otherwise       = empty

lookups :: forall ix a . Ix ix => Array ix a -> [ix] -> [a]
lookups m = catMaybes . takeWhile isJust . fmap (m !?)

line :: Ix ix => (ix -> ix) -> ix -> Array ix a -> [a]
line d ix m = lookups m (iterate d ix)

type Direction = (Int, Int) -> (Int, Int)
east, west, north, south, northeast, northwest, southeast, southwest :: Direction
east (a, b) = (a, succ b)
west (a, b) = (a, pred b)
north (a, b) = (pred a, b)
south (a, b) = (succ a, b)
northeast (a, b) = (pred a, succ b)
northwest (a, b) = (pred a, pred b)
southeast (a, b) = (succ a, succ b)
southwest (a, b) = (succ a, pred b)

directions :: [Direction]
directions = [north, south, east, west, northeast, northwest, southeast, southwest]
