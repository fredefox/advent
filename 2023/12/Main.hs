{-# language OverloadedStrings, TypeApplications, MultiWayIf, ViewPatterns, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  xs <- parse <$> Text.getContents
  print $ sum $ length . uncurry solve <$>  xs

solve :: String -> [Int] -> [String]
solve xs = \case
  [] -> if all blankable xs then pure (replicate (length xs) '.') else empty
  (n:ns) -> if
    | n <= length xs -> startsHere <|> skipOne xs (n:ns)
    | otherwise -> empty
    where
    startsHere = case splitAt n xs of
      (_, '#':_) -> empty
      (as, _:bs) -> guard (all blockable as) *> do { r <- solve bs ns ; pure (replicate n '#' <> "." <> r) }
      (as, bs) -> guard (all blockable as) *> solve bs ns *> pure (replicate n '#')

skipOne :: String -> [Int] -> [String]
skipOne xs ns = case xs of
  [] -> empty
  (x:xss) -> guard (blankable x) *> do { r <- solve xss ns ; pure ("." <> r) }

blankable :: Char -> Bool
blankable = \case
    '?' -> True
    '.' -> True
    _ -> False

blockable :: Char -> Bool
blockable = \case
    '#' -> True
    '?' -> True
    _ -> False

parse :: Text -> [(String, [Int])]
parse = fmap (go . Text.words) . Text.lines
  where
  go (x:y:_) = (Text.unpack x, tread @Int <$> Text.splitOn "," y)
  go _ = error "Parse error"

tread :: forall a . Read a => Text -> a
tread = read @a . Text.unpack
