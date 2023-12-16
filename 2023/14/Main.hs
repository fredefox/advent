{-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.List as List

main :: IO ()
main = do
  xs <- lines <$> getContents
  let ys = fmap tilt $ List.transpose xs
  print $ sum $ foldMap weights ys

weights :: String -> [Int]
weights xs = fmap fst $ filter (\(_, c) -> c == 'O') $ zip [1..] $ reverse xs

tilt :: String -> String
tilt = foldMap (left (== '#') . left (== 'O')) . chunks (span (/= '#'))

chunks :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunks f xs = a : case bs of
  [] -> mempty
  (b:bss) -> b `first` chunks f bss
  where
  (a, bs) = f xs

first :: a -> [[a]] -> [[a]]
x `first` [] = [[x]]
x `first` (xs:xxs) = (x:xs):xxs

left :: (a -> Bool) -> [a] -> [a]
left p xs = filter p xs <> filter (not . p) xs
