{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
import Prelude hiding (elem)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Internal as Map
import Control.Monad
import Data.Foldable (traverse_)
import Debug.Trace

main :: IO ()
main = do
  (m, mine, others) <- parse <$> Text.getContents
  let is = unions $ Map.elems m
  -- print is
  -- print others
  let invalidFields = foldMap (filter (not . (`elem` is))) others
  print $ sum invalidFields
  let validTickets = filter (all ((`elem` is))) others
  -- traverse_ print validTickets
  -- forM_ others $ \other -> print $ filter (not . (`elem` is)) other
  print m
  print $ head validTickets
  print $ validAssignments (Map.toList m) $ head validTickets
  print $ validAssignments (Map.toList m) [3]

type Spec = (Map Text (Intervals Int), [[Int]], [[Int]])

parse :: Text -> Spec
parse xs = (Map.fromList s, go myTicket, go nearby)
  where
  (spec : myTicket : nearby : _) = Text.lines <$> Text.splitOn "\n\n" xs
  go s' = fmap (read @Int . Text.unpack) . Text.splitOn "," <$> tail s'
  s = q . Text.splitOn ": " <$> spec
  q (x:y:_) = (x, fromList $ r . Text.splitOn "-" <$> Text.splitOn " or " y)
  q _ = error "Parse error"
  r (a : b : _) = (read @Int $ Text.unpack a, read @Int $ Text.unpack b)
  r _ = error "Parse error"

type Intervals a = Map a a

insert :: forall a . Ord a => (a, a) -> Intervals a -> Intervals a
insert (a, b) m = ma' `Map.union` mb
  where
  (ma, mb) = Map.spanAntitone (<= a) m
  ma' = replaceMaxWithKey f ma
  f :: Maybe (a, a) -> (a, a)
  f Nothing              = (a, b)
  f (Just bnds@(a',  b'))
    -- | a' <= a && a <= b' = (a', max b b')
    | a `inBounds` bnds  = (a', max b b')
    | otherwise          = (a, b)
  -- ma' = case Map.lookupMax ma of
  --   Nothing -> Map.singleton a b
  --   Just (a', b')
  --     | a' <= a && a <= b' -> _
  --     | otherwise          -> _

replaceMaxWithKey :: Ord k => (Maybe (k, v) -> (k, v)) -> Map k v -> Map k v
replaceMaxWithKey f m = Map.insertMax k v $ Map.deleteMax m
  where
  (k, v) = f $ Map.lookupMax m

fromList :: Ord a => [(a, a)] -> Intervals a
fromList = foldr insert mempty

unions :: Foldable t => Ord a => t (Intervals a) -> Intervals a
unions = fromList . foldMap Map.toList

elem :: Ord a => a -> Intervals a -> Bool
elem a m = case Map.lookupLT a m of
  Nothing -> r
  Just b -> a `inBounds` b || r
  where
  r = case Map.lookup a m of
    Nothing -> False
    Just{}  -> True

inBounds :: Ord a => a -> (a, a) -> Bool
inBounds a (x, y) = x <= a && a <= y

type Assignment = [] Text

type Ticket = [] Int

validAssignments :: [(Text, Intervals Int)] -> Ticket -> [] Assignment
validAssignments _ [] = [[]]
validAssignments xs (n:nss) = do
  let (used, unused) = f n
  (label,_) <- used
  assignments <- validAssignments (traceShowId unused) nss
  pure $ label : assignments
  where
  f n = (a, b)
    where
    a = filter (elem n . snd) xs
    b = filter (not . elem n . snd) xs
