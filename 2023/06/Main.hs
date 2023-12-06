{-# language ViewPatterns, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

-- Unidentified "two-off" error somwhere in the code.

import GHC.Float

main :: IO ()
main = do
  contents <- getContents
  print $ part1 contents
  part2 contents

part2 :: String -> IO ()
part2 contents = do
  print $ solve t d
  where
  (t, d)
    = \case { (t':d':_) -> (t', d') ; _ -> error "Parse error" }
    $ fmap (read @Int)
    <$> lines
    $ filter (/= ' ') contents

part1 :: String -> Int
part1 contents = case fmap (fmap (read @Int) . words) . lines $ contents of
  (ts:ds:_) -> product $ zipWith solve ts ds
  _ -> error "Parse error"

solve :: Int -> Int -> Int
solve (int2Float -> t) (int2Float -> k) = n - tt0 - tt1
  where
  (l, u) = (f t k, g t k)
  n = succ $ floor u - ceiling l
  tt0, tt1 :: Int
  tt0 = fromEnum $ int2Float (ceiling l) == l
  tt1 = fromEnum $ int2Float (floor u) == u

f, g :: Float -> Float -> Float
f t k = ((- t + sqrt ( t ** 2 - 4 * k)) / (-2))
g t k = ((- t - sqrt ( t ** 2 - 4 * k)) / (-2))

