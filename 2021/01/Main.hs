{-# language TypeApplications, LambdaCase #-}

import qualified Data.List

main :: IO ()
main = do
  xs <- fmap (read @Int) . lines <$> getContents
  print $ part1 xs
  let n = 3
  print $ part1 $ fmap sum $ takeWhile ((>= n) . length) $ take n <$> Data.List.tails xs

part1 :: Ord a => [] a -> Int
part1 = \case
  [] -> 0
  xs@(_:xss) -> length $ filter id $ zipWith (<) xs xss
