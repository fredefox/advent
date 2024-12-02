module Main (main) where

main :: IO ()
main = do
  xs <- parse <$> getContents
  print $ part1 xs
  print $ part2 xs

drop1 :: [a] -> [[a]]
drop1 [] = []
drop1 (x:xs) = xs : fmap (x :) (drop1 xs)

part2 :: [[Int]] -> Int
part2 = length . filter (any safe . drop1)

part1 :: [[Int]] -> Int
part1 xs = length $ filter safe xs

safe :: [Int] -> Bool
safe xs = mon xs && mx xs

mx :: [Int] -> Bool
mx [] = True
mx xs@(_:xss)
  = all (\n -> abs n <= 3)
  $ zipWith (-) xs xss

mon :: [Int] -> Bool
mon xs = m (<) || m (>)
  where
  m p = and $ zipWith p xs (tail xs)

parse :: String -> [[Int]]
parse = fmap (fmap (read @Int) . words) . lines
