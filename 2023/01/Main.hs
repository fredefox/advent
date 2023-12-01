{-# language TypeApplications #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Data.Char (isDigit)
import Data.Maybe

main :: IO ()
main = do
  xs <- getContents
  part1 xs >> part2 xs

part2 :: String -> IO ()
part2 contents = do
  let xs = lines $ contents
  let ns = fmap ((\l -> head l * 10 + last l) . parse) xs
  print $ sum ns

-- Parse one line:
-- >>> parse "two1nine"
-- [2, 1, 9]
-- >>> parse "eightwo"
-- [8, 2]
parse :: String -> [Int]
parse [] = []
parse (x:xs) | isDigit x = read @Int [x] : parse xs
parse s = case findJust (\(k, _) -> s `beginsWith` k) tbl of
  Nothing -> parse (tail s)
  Just (k, v) -> v : parse (drop (length k) s)
  where
  tbl =
    [ "zero"  |> 0
    , "one"   |> 1
    , "two"   |> 2
    , "three" |> 3
    , "four"  |> 4
    , "five"  |> 5
    , "six"   |> 6
    , "seven" |> 7
    , "eight" |> 8
    , "nine"  |> 9
    ]
  (|>) = (,)

findJust :: (a -> Bool) -> [a] -> Maybe a
findJust p = listToMaybe . filter p

beginsWith :: String -> String -> Bool
a `beginsWith` b = take (length b) a == b

part1 :: String -> IO ()
part1 contents = do
  let f l = [head l, last l]
      g = read @Int . f . filter isDigit
  print $ sum $ fmap g . lines $ contents
