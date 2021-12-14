{-# language LambdaCase #-}
{-# language TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Foldable
import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Bag (Bag)
import qualified Bag

main :: IO ()
main = do
  inp <- getContents
  let (s, m) = parse inp
  traverse_ (print . Data.List.sortOn snd . Bag.toList . Bag.fromList . fmap (\((a, _), n) -> (a, n)) . Bag.toList) $ iterate (go m) s

type P = (Char, Char)

go :: M -> B -> B
go m s = Bag.fromList $ foldMap f $ Bag.toList s
  where
  f :: (P, Int) -> [(P, Int)]
  f (p@(a, b), n) = case Map.lookup p m of { Nothing -> [] ; Just c -> [((a, c), n), ((c, b), n)] }

type M = Map (Char, Char) Char
type B = Bag Int (Char, Char)

parse :: String -> (B, M)
parse inp = case lines inp of
  (x:_:xs) -> (s, Map.fromList $ fmap ((\case { Just ((a:b:_), (c:_)) -> ((a, b), c) ; _ -> undefined ;  }) . splitOn " -> ") xs)
    where
    s :: Bag Int (Char, Char)
    s = foldMap @_ @(Bag _ _) (\case { (a:b:_) -> Bag.singleton (a, b) 1 ; _ -> mempty}) (Data.List.tails x)
  _ -> undefined

splitOn :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitOn s x = f <$> splitOn' s x
  where
  f (a, b) = (a, drop (length s) b)

-- | @splitOn'` but retain the delimiter:
-- >>> splitOn' "," "a,b"
-- Just ("a", ",b")
splitOn' :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitOn' s x = find f $ splits x
  where
  f (_, y) = s `Data.List.isPrefixOf` y

splits :: [a] -> [([a], [a])]
splits xs = zip (Data.List.inits xs) (Data.List.tails xs)
