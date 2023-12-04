{-# options_ghc -Wall #-}
module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Semigroup

main :: IO ()
main = do
  gs <- fmap parse . lines <$> getContents
  print $ part1 gs
  print $ part2 gs

part2 :: [G] -> Int
part2 gs = sum $ solve2 <$> gs

solve2 :: G -> Int
solve2 (_, xs) = case foldMap go xs of
  (a, b, c) -> getMax $ a * b * c
  where
  go x = (q "red", q "green", q "blue")
    where
    q c = Max $ fromMaybe 0 $ lookup c x

part1 :: [G] -> Int
part1 gs = sum $ fmap go $ filter solve gs
  where
  go (x, _) = case words x of
    _:y:_ -> read @Int y
    _ -> error "parse error"

type G = (String, [[(String, Int)]])

solve :: G -> Bool
solve (_, x) = all p x
  where
  p :: [(String, Int)] -> Bool
  p l
    =  q "red"   12
    && q "green" 13
    && q "blue"  14
    where
    q :: String -> Int -> Bool
    q k n = case lookup k l of { Nothing -> True ; Just m -> m <= n}

parse :: String -> G
parse s = case split (== ':') s of
  x:y:_ -> (x, fmap p . split (== ',') <$> split (== ';') y)
  _ -> error "Parse error"
  where
  p s' = case words s' of
    n:c:_ -> (c, read @Int n)
    _ -> error "Parse error"

split :: (Char -> Bool) -> String -> [String]
split p xs = case break p xs of
  (a, b) -> a : case b of { [] -> [] ; (_:bs) -> split p bs}
