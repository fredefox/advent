{-# options_ghc -Wall #-}
module Main (main) where

main :: IO ()
main = do
  xs <- parse <$> getContents
  print $ sum $ next <$> xs
  print $ sum $ next . reverse <$> xs

parse :: String -> [[Int]]
parse = fmap (fmap (read @Int) . words) . lines

next :: Integral n => [n] -> n
next x = sum $ last <$> diffs x

diffs :: Integral n => [n] -> [[n]]
diffs
  = takeWhile (any (/= 0))
  . iterate diff
  where
  diff xs = zipWith subtract xs (tail xs)
