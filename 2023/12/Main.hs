{-# language OverloadedStrings, TypeApplications, MultiWayIf, ViewPatterns #-}
module Main (main) where

import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Applicative

import Debug.Trace as Trace

-- trace _ x = x
-- traceShowId = id

main :: IO ()
-- main = traverse_ print $ solve [('?', 3), ('.', 1)] [1,1]
-- main = traverse_ print $ solve [('?', 3), ('.', 1), ('#', 3)] [1,1,3]
main = traverse_ print $ uncurry solve $ head $ parse "#?#? 1,2"
-- main = traverse_ print $ uncurry solve $ head $ parse "#? 2"
-- main = traverse_ print $ uncurry solve $ head $ parse "# 1"
-- main
--   = traverse_ print
--   $ solve' [('?',1),('#',2)] [3]

-- main = print $ solve [('.', 1)] []
-- main = print $ solve [('?', 1)] [1]
-- main = do
--   xs <- parse <$> Text.getContents
--   traverse_ print xs
--   traverse_ (print . length . uncurry solve) xs

-- solve ???.### 1 1 3
-- = solve #??.### 1 1 3 <|> solve .??.### 1 1 3

-- solve #??? 1 = solve' ??? 0
-- solve #??? 2 = solve' ??? 1
-- solve #??? 5 = solve' ??? 4
solve :: [(Char, Int)] -> [Int] -> [[(Char, Int)]]
solve (trace "solve" -> traceShowId -> ((c, n):xs)) (traceShowId -> (m:ms)) = case c of
  '?' -> solve' ((c,n):xs) (m:ms) <|> ((compactSim . (('.', 1):)) <$> solve (compactZero $ (c, pred n):xs) (m:ms))
  -- '#' -> solve' xs ms
  '#' -> solve' ((c, n) : xs) (m:ms)
  _   -> ((c, n):) <$> solve xs (m:ms)
solve [] [] = pure []
solve (('.', n):xs) [] = (('.',n):) <$> solve xs []
solve (('?', n):xs) [] = (('.',n):) <$> solve xs []
-- solve [] [] = pure []
solve _ _ = empty

-- solve' ?? 2 = solve' ## 2
-- solve' ??? 2 = solve' ##. 2
-- solve' ???? 2 = solve' ##.? 2
solve' :: [(Char, Int)] -> [Int] -> [[(Char, Int)]]
solve' (trace "solve'" -> traceShowId -> t) (traceShowId -> u) = go t u
  where
  go (('?', n):_xs) (_m:_ms) | n == 0 = error "..."
  go (('?', n):xs) (m:ms) | m >= n = solve' (compactSim $ ('#', n):xs) (m:ms)
  go (('?', n):xs) (m:ms) = solve' (('#', m):compactSim (('.',1):(compactZero $ ('?', n-m-1):xs))) (m:ms)
  go (('#', n):xs) (m:ms) = if
    | n == m -> trace "match" $ (('#', n):) <$> solve xs ms
    | n <= m -> trace "pre-match" $ (compactSim . (('#', n):)) <$> solve' xs (m - n : ms)
    | otherwise -> trace "no match" empty
  go _ _ = empty
  -- go ((c, n):xs) (m:ms) = empty

compactZero ((_, 0):xs) = xs
compactZero xs = xs

compactSim ((c0, n0):(c1, n1):xs) | c0 == c1 = (c0, n0 + n1):xs
compactSim xs = xs

-- parse :: String ->
parse = fmap (go . Text.words) . Text.lines
  where
  go (x:y:_) = (fmap row $ NonEmpty.group $ Text.unpack x, tread @Int <$> Text.splitOn "," y)
  go _ = error "Parse error"
  row x@(c :| _) = (c, length x)

tread :: forall a . Read a => Text -> a
tread = read @a . Text.unpack
