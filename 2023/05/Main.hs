{-# language OverloadedStrings, ViewPatterns #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  (source, maps) <- parse <$> Text.getContents
  print $ minimum $ run maps <$> source

run :: [[[Int]]] -> Int -> Int
run maps = f
  where
  fs = funcify <$> reverse maps
  f = foldr (.) id fs

parse :: Text -> ([Int], [[[Int]]])
parse s = case Text.splitOn "\n\n" s of
  (x:xs) -> (fmap (textRead @Int) <$> Text.words $ Text.splitOn ":" x !! 1, parseMap . Text.strip . (!! 1) . Text.splitOn ":" <$> xs)
  _ -> error "Parse error"

-- 50 98 2
-- 52 50 48
parseMap :: Text -> [[Int]]
parseMap = fmap (fmap textRead . Text.words) . Text.lines

textRead :: forall a . Read a => Text -> a
textRead = read @a . Text.unpack

funcify :: [[Int]] -> Int -> Int
funcify w n = fromMaybe n $ asum $ one n <$> w

one :: Int -> [Int] -> Maybe Int
one n w = case w of
  (r:l:k:_) | n `between` (l, l + k) -> pure $ r + (n - l)
  _ -> empty

between :: Int -> (Int, Int) -> Bool
x `between` (a, b) = a <= x && x < b
