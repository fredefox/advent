module Main (main) where

import Data.Foldable (traverse_)

main :: IO ()
main = do
  gs <- fmap parse . lines <$> getContents
  part1 gs

part1 gs = do
  print $ sum $ fmap go $ filter solve gs
  where
  go (x, _) = case words x of
    _:y:_ -> read @Int y
    _ -> error "parse error"

solve :: (String, [[(String, Int)]]) -> Bool
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

parse :: String -> (String, [[(String, Int)]])
parse s = case split (== ':') s of
  x:y:_ -> (x, fmap p . split (== ',') <$> split (== ';') y)
  _ -> error "Parse error"
  where
  -- p :: String -> (String, Int)
  p s = case words s of
    n:c:_ -> (c, read @Int n)
    _ -> error "Parse error"

split :: (Char -> Bool) -> String -> [String]
split p xs = case break p xs of
  (a, b) -> a : case b of { [] -> [] ; (_:bs) -> split p bs}
