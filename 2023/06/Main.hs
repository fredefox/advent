{-# language ViewPatterns #-}
{-# options_ghc -Wall #-}
module Main (main) where

import GHC.Float

main :: IO ()
main = do
  (ts:ds:_) <- fmap (fmap (read @Int) . words) . lines <$> getContents
  print $ product $ zipWith solve ts ds

solve :: Int -> Int -> Int
solve (int2Float -> t) (int2Float -> k) = n - tt0 - tt1
  where
  (l, u) = (f t k, g t k)
  n = succ $ floor u - ceiling l
  tt0, tt1 :: Int
  tt0 = fromEnum $ int2Float (ceiling l) == l
  tt1 = fromEnum $ int2Float (ceiling u) == u

f, g :: Float -> Float -> Float
f t k = ((- t + sqrt ( t ** 2 - 4 * k)) / (-2))
g t k = ((- t - sqrt ( t ** 2 - 4 * k)) / (-2))

