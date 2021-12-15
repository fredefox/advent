{-# language ScopedTypeVariables #-}
module Main (main, showMatrix) where

import Data.Char
import Control.Monad
import Data.Ix (Ix)
import qualified Data.Ix as Ix
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Foldable

main :: IO ()
main = do
  xs <- fmap (fmap digitToInt) . lines <$> getContents
  let a = fromMatrix xs
  print $ solve a
  -- print $ Array.bounds a
  let b = growA 5 a
  -- putStr $ showMatrix b
  print $ solve b

solve :: forall n . Integral n => Array (Int, Int) n -> n
solve a = b ! ix1 - b ! ix0
  where
  b = mkArray a
  ix0, ix1 :: (Int, Int)
  (ix0, ix1)  = Array.bounds a

mkArray :: forall n . Integral n => Array (Int, Int) n -> Array (Int, Int) n
mkArray a = b
  where
  bounds :: ((Int, Int), (Int, Int))
  bounds  = Array.bounds a
  b :: Array (Int, Int) n
  b = array bounds f
  f :: (Int, Int) -> n
  f ix@(n, m)
    | n == 0 && m == 0 = a ! ix
    | n == 0           = (a ! ix) +                       (b ! (n, pred m))
    |           m == 0 = (a ! ix) +     (b ! (pred n, m))
    | otherwise        = (a ! ix) + min (b ! (pred n, m)) (b ! (n, pred m))

array :: Ix ix => (ix, ix) -> (ix -> e) -> Array ix e
array b f  = Array.array b (f' <$> Ix.range b)
  where
  f' i = (i, f i)

fromMatrix :: [[a]] -> Array (Int, Int) a
fromMatrix xs = Array.listArray ((0, 0), (n, m)) (join xs)
  where
  n = pred $ length xs
  m = pred $ case xs of { [] -> 0 ; (x:_) -> length x }

showMatrix :: Show a => Array (Int, Int) a -> String
showMatrix a = unlines $ fmap (unwords . fmap show) $ chunksOf (succ n) $ Array.elems a
  where
  (_, (n, _)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

growA :: Int -> Array (Int, Int) Int -> Array (Int, Int) Int
growA k a = array ((0, 0), (pred $ n * k, pred $ m * k)) f
  where
  n = succ n'
  m = succ m'
  (_, (n', m')) = Array.bounds a
  f = grow a

-- | We don't need to allocate the "grown" matrix!
grow :: Array (Int, Int) Int -> (Int, Int) -> Int
grow a (i, j) = transf 0 10 ((a ! (i `mod` n, j `mod` m)) + (i `div` n) + (j `div` m))
  where
  n = succ n'
  m = succ m'
  (_, (n', m')) = Array.bounds a

-- | Handling wrap-around at 1 was a bit awkward for me, hence this
-- function. `transf 0 10` handles wrapping around at 1. The recursive
-- calls account for the extra 1 step we need to take compared to if
-- we just used "regular" modular arithmetic. `transf 0 3` has the graph:
-- 0  0
-- 1  1
-- 2  2
-- 3  1
-- 4  2
transf :: Int -> Int -> Int -> Int
transf a b x = if p == 0 then q else transf a b (p + q)
  where
  (p, q) = (x - a) `divMod` (b - a)
