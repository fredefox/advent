{-# language OverloadedStrings #-}
-- {-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable

main :: IO ()
main = do
  xs <- Text.splitOn "," <$> Text.getContents
  print $ sum $ hash . Text.unpack <$> xs

hash :: Enum a => [a] -> Int
hash = foldl' go 0
  where
  go a acc = 17 * (fromEnum acc + a) `rem` 256
