{-# language StandaloneDeriving, DerivingStrategies, MultiWayIf, LambdaCase, ViewPatterns #-}
{-# options_ghc -Wall #-}
module Main (main, toString) where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Tuple (swap)
import qualified Data.List as List

main :: IO ()
main = do
  c <- fmap (go . words) . lines <$> getContents
  let xs = List.sortOn (\(h, _) -> score h) c
  print $ sum $ (\((_h, n), r) -> n * r) <$> zip xs [1..]
  where
  go = \case
    [h, n] -> (fromString h, read @Int n)
    _ -> error "Parse error"

newtype Card = Card Int

deriving newtype instance Eq Card
deriving newtype instance Ord Card
deriving newtype instance Show Card

newtype Hand = Hand [Card]
deriving newtype instance Eq Hand
deriving newtype instance Ord Hand
deriving newtype instance Show Hand

data Label where
  HighCard  :: Label
  OnePair   :: Label
  TwoPair   :: Label
  Three     :: Label
  FullHouse :: Label
  Four      :: Label
  Five      :: Label

deriving stock instance Show Label
deriving stock instance Eq Label
deriving stock instance Ord Label

label :: Hand -> Label
label (Hand h) = case grps of
  [_:_]                    -> Five
  [[_], [_,_,_,_]]         -> Four
  [[_,_], [_,_,_]]         -> FullHouse
  [_:_, _:_, [_,_,_]]      -> Three
  [_:_, [_,_], [_,_]]      -> TwoPair
  [_:_, _:_, _:_, [_, _]]  -> OnePair
  _                        -> HighCard
  where
  grps = jokers $ List.sortOn length $ List.group $ List.sort h

jokers :: [[Card]] -> [[Card]]
jokers h = init_njs <> [replicate numjs l <> last_njs]
  where
  joker (Card n) = n == 0
  js :: [[Card]]
  js = filter (joker . head) h
  numjs = case js of
    [] -> 0
    (x:_) -> length x
  njs = filter (not . joker . head) h
  last_njs = case njs of { [] -> mempty ; _ -> last njs }
  init_njs = case njs of { [] -> mempty ; _ -> init njs }
  l = head last_njs

data Score = Score Label Hand

deriving stock instance Show Score
deriving stock instance Eq Score
deriving stock instance Ord Score

score :: Hand -> Score
score h = Score (label h) h

fromString :: String -> Hand
fromString = Hand . fmap fromChar

toString :: Hand -> String
toString (Hand h) = toChar <$> h

fromChar :: Char -> Card
fromChar c = case c `lookup` table of
  Nothing | Char.isDigit c -> Card $ Char.digitToInt c
  Nothing -> error "Parse error"
  Just n -> Card n

table :: [(Char, Int)]
table =
    [ 'A' |> 14
    , 'K' |> 13
    , 'Q' |> 12
    , 'J' |> 0
    , 'T' |> 10
    ]
  where
  (|>) = (,)

toChar :: Card -> Char
toChar (Card n) = Maybe.fromMaybe (Char.intToDigit n) $ lookup n m
  where
  m = fmap swap table
