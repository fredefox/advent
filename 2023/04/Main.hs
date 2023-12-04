{-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.Set as Set
import Data.Bits (shift)

main :: IO ()
main = do
  gs <- fmap parse . lines <$> getContents
  -- traverse_ print gs
  print $ sum $ solve <$> gs

type G = (Int, ([Int], [Int]))

solve :: G -> Int
solve (_, (a, b)) = (1 :: Int) `shift` pred n
  where
  n = Set.size $ Set.fromList a `Set.intersection` Set.fromList b

parse :: String -> G
parse s = case split (== ':') s of
  g:l:_ -> (pp, qq)
    where
    pp = case words g of { _:n:_ -> read @Int n ; _ -> error "Parse error" }
    qq =
      case split (== '|') l of
        a:b:_ -> (tt a, tt b)
        _ -> error "Parse error"
        where
        tt = fmap (read @Int) . words
  _ -> error "Parse error"

split :: (Char -> Bool) -> String -> [String]
split p xs = case break p xs of
  (a, b) -> a : case b of { [] -> [] ; (_:bs) -> split p bs}
