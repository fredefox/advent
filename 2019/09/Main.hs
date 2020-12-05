{-# language LambdaCase #-}
import Data.Foldable
import Data.Array
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  ls <- lines <$> getContents
  let r = buildArray ls
  let ps = fmap fst $ filter ((== '#') . snd) $ assocs r
  let ranked = sortOn snd $ (\p -> (p, Set.size $ center p (filter (/= p) ps))) <$> ps
  -- traverse_ print ranked
  let (p, _) = last ranked
  -- traverse_ print $ sortOn (uncurry angle) $ Set.toList $ center p (filter (/= p) ps)
  let byAngle = fmap snd $ sortOn fst $ fmap (\(a, b) -> (uncurry angle a, b)) $ Map.toList $ center' p (filter (/= p) ps)
  -- traverse_ print byAngle
  -- print p
  traverse_ print $ join $ transpose byAngle
  -- traverse_ print $ sortOn snd $ fmap (\x -> (x, uncurry angle x)) $ Set.toList $ center p (filter (/= p) ps)

center :: (Int, Int) -> [(Int, Int)] -> Set (Int, Int)
center (n, m) = Set.fromList . fmap go
  where
  go (i, j) = (i - n) `norm` (j - m)

center' :: (Int, Int) -> [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
center' (n, m) = Map.map (sortOn (\(n0, _) -> negate $ gcd n n0)) . Map.unionsWith (<>) . fmap go
  where
  go :: (Int, Int) -> Map (Int, Int) [(Int, Int)]
  go (i, j) = let p = (i - n) `norm` (j - m) in Map.singleton p [(i, j)]

angle :: Int -> Int -> Double
angle n m = if r < 0 then r + pi else r
  where r = atan2 (fromIntegral n) (fromIntegral m) - atan2 (-1) 0

norm :: Int -> Int -> (Int, Int)
norm a b = (a `div` x, b `div` x) where x = gcd a b

buildArray :: [[a]] -> Array (Int, Int) a
buildArray = \case
  [] -> listArray ((0, 0), (0, 0)) []
  xs@(x:_) -> listArray ((0, 0), (pred $ length x, pred $ length xs)) $ join xs
