{-# language StandaloneDeriving, DerivingStrategies, MultiWayIf, LambdaCase #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Tuple (swap)
import Data.Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

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

-- data Score where
--   Five :: Card -> Score
--   Four :: Card -> Card -> Score
--   FullHouse :: Card -> Card -> Score
--   Three :: Card -> Card -> Card -> Score
--   TwoPair :: Card -> Card -> Card -> Score
--   OnePair :: Card -> Card -> Card -> Card -> Score
--   HighCard :: Hand -> Score
--
-- score :: Hand -> Score
-- score (Hand h) = case grps of
--   [x:_] -> Five x
--   [[x], [y,_,_,_]] -> Four y x
--   [x:_, [y,_,_]] -> FullHouse y x
--   [x:_, y:_, [z,_,_]] -> Three z (x `max` y) (x `min` y)
--   [x:_, [y,_], [z,_]] -> TwoPair (y `max` z) (y `min` z) x
--   [x:_, y:_, z:_, [zz, _]] -> OnePair zz x y z
--   _ -> HighCard $ Hand h
--   where
--   grps = List.sortOn length $ List.group $ List.sort h
--   n = length grps

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
  [x:_]                    -> Five
  [[x], [y,_,_,_]]         -> Four
  [x:_, [y,_,_]]           -> FullHouse
  [x:_, y:_, [z,_,_]]      -> Three
  [x:_, [y,_], [z,_]]      -> TwoPair
  [x:_, y:_, z:_, [zz, _]] -> OnePair
  _                        -> HighCard
  where
  grps = List.sortOn length $ List.group $ List.sort h
  n = length grps

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
    , 'J' |> 11
    , 'T' |> 10
    ]
  where
  (|>) = (,)

toChar :: Card -> Char
toChar (Card n) = Maybe.fromMaybe (Char.intToDigit n) $ lookup n m
  where
  m = fmap swap table
