{-# language OverloadedStrings, MultiWayIf, ViewPatterns, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  instr:_:t <- Text.lines <$> Text.getContents
  let
    m = Map.fromList
      $ (\case {(a:b:c:_) -> (a, (b, c)) ; _ -> error "Parse error"})
      . Text.words <$> t
  let path = search "AAA" "ZZZ" (cycle $ Text.unpack instr) m
  print $ length path

search :: Text -> Text -> String -> Map Text (Text, Text) -> [Text]
search start end instructions m = go instructions start
  where
  go :: String -> Text -> [Text]
  go [] _ = error "IMPOSSIBLE"
  go (t:ts) current = current : if
    | current == end -> mempty
    | otherwise -> case current `Map.lookup` m of
        Just (l, r) -> case t of
          'R' -> go ts r
          _ -> go ts l
        Nothing -> error "IMPOSSIBLE"
