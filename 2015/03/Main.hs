{-# language LambdaCase #-}
module Main (main) where

import qualified Data.Set as Set

main :: IO ()
main = do
  xs <- getContents
  print $ Set.size $ solve xs
  let (xs0, xs1) = uninterleave xs
  putStr "Incorrect: "
  print $ Set.size $ solve xs0 <> solve xs1

uninterleave :: [a] -> ([a], [a])
uninterleave = go ([], [])
  where
  go (xs0, xs1) (x0:x1:xs) = go (x0:xs0, x1:xs1) xs
  go acc _ = acc

solve :: [Char] -> Set.Set (Int, Int)
solve xs = s
  where
  ys = scanl step (0, 0) xs
  s = Set.fromList ys

step :: (Int, Int) -> Char -> (Int, Int)
step (n, m) = \case
  '>' -> (succ n, m)
  '<' -> (pred n, m)
  '^' -> (n, succ m)
  'v' -> (n, pred m)
  _   -> (n, m)
