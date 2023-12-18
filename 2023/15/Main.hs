{-# language OverloadedStrings, TypeApplications, DerivingStrategies #-}
{-# options_ghc -Wall #-}
module Main (main) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Monoid

type Map a b = [(a, b)]

main :: IO ()
main = do
  c <- Text.getContents
  let xs = Text.splitOn "," c
  print $ sum $ hash . Text.unpack <$> xs
  let m = interps (parse c) mempty
  print $ eval m

eval :: IntMap (Map String Int) -> Int
eval = getSum . foldMap (uncurry go) . IntMap.toList
  where
  go :: Int -> Map String Int -> Sum Int
  go n mp = Sum (succ n) * m
    where
    m = foldMap Sum . zipWith (*) [1..] . fmap snd . reverse $ mp

interps :: [S] -> IntMap (Map String Int) -> IntMap (Map String Int)
interps xs s = foldl (flip interp) s xs

interp :: S -> IntMap (Map String Int) -> IntMap (Map String Int)
interp s m = case s of
  Add l n -> IntMap.alter (pure . ins l n . fromMaybe mempty) (hash l) m
  Rem l -> IntMap.alter (Just . del l . fromMaybe mempty) (hash l) m

ins :: forall a b . Ord a => a -> b -> Map a b -> Map a b
ins l n m = case List.lookup l m of
  Nothing -> (l, n) : m
  Just{} -> fmap (\(l', n') -> if l' == l then (l, n) else (l', n')) m

del :: String -> Map String Int -> Map String Int
del l = filter ((/= l) . fst)

hash :: Enum a => [a] -> Int
hash = foldl' go 0
  where
  go a acc = 17 * (fromEnum acc + a) `rem` 256

data S = Add String Int | Rem String

deriving stock instance Show S

parse :: Text -> [S]
parse = fmap go . Text.splitOn ","
  where
  go s = case Text.splitOn "=" s of
    (l:i:_) -> Add (Text.unpack l) $ read @Int $ Text.unpack i
    (l:_) -> Rem $ Text.unpack $ Text.replace "-" "" l
    _ -> error "Parse error"
