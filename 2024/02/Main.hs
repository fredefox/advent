module Main (main) where

main :: IO ()
main = do
  xs <- parse <$> getContents
  -- print $ fmap diffs xs
  -- print $ fmap safe xs
  print $ part1 xs

part1 :: [[Int]] -> Int
part1 xs = length $ filter safe xs

safe :: [Int] -> Bool
safe xs = mon xs && mx xs

mx :: [Int] -> Bool
mx [] = True
mx xs@(_:_) = all (\n -> abs n <= 3) $ diffs xs

mon :: [Int] -> Bool
mon [] = True
mon xs@(_:_) = all p ds
  where
  (d:ds) = diffs xs
  p | d > 0     = (> 0)
    | d < 0     = (< 0)
    | otherwise = const False

diffs :: [Int] -> [Int]
diffs xs@(_:xss) = zipWith (-) xs xss

parse :: String -> [[Int]]
parse = fmap (fmap (read @Int) . words) . lines
