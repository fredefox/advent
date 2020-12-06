{-# language TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language ScopedTypeVariables #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable
import Data.Coerce
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
   vs <- getInput
   -- pt1 vs
   let xs = coerce @[[V3 Int]] @[[[(Int, Int)]]] $ iterate step vs
   let go n = pt2 $ fmap (fmap (!! n)) xs 
   print $ foldr lcm 1 [go 0, go 1, go 2]

pt1 :: [V3 Int] -> IO ()
pt1 vs = traverse_ print $ energies <$> iterate step vs

pt2 :: forall a . Ord a => [a] -> Int
pt2 = go mempty 0
  where
  go :: Set a -> Int -> [a] -> Int
  go s n (x:xs) = if Set.member x s then n else go (Set.insert x s) (succ n) xs

getInput :: IO [V3 Int]
getInput = fmap parse . Text.lines <$> Text.getContents

gravity :: V3 Int -> V3 Int -> V3 Int
gravity (V3 xs) (V3 ys) = V3 $ zipWith go xs ys
  where
  go :: (Sum Int, Sum Int) -> (Sum Int, Sum Int) -> (Sum Int, Sum Int)
  go (x, _) (y, dy) = (y, dy')
    where
    dy' = case compare x y of
      LT -> coerce (pred @Int) dy
      EQ -> dy
      GT -> coerce (succ @Int) dy

gravities :: [V3 Int] -> [V3 Int]
gravities xs = do
  x <- xs
  pure $ foldr gravity x xs

velocity :: V3 Int -> V3 Int
velocity (V3 xs) = V3 $ fmap go xs
  where
  go :: (Sum Int, Sum Int) -> (Sum Int, Sum Int)
  go (x, dx) = (x <> dx, dx)

velocities :: [V3 Int] -> [V3 Int]
velocities = fmap velocity

step :: [V3 Int] -> [V3 Int]
step = velocities . gravities

energy :: V3 Int -> Int
energy xs' = kin * pot
  where
  xs :: [(Int, Int)]
  xs = coerce xs'
  kin = sum $ fmap (abs . fst) xs
  pot = sum $ fmap (abs . snd) xs

energies :: [V3 Int] -> Int
energies = sum . fmap energy

newtype V3 a = V3 [(Sum a, Sum a)]

deriving stock instance Show a => Show (V3 a)

instance Num a => Semigroup (V3 a) where
  V3 a <> V3 b = V3 $ zipWith (<>) a b

parse :: Text -> V3 Int
parse = go . fmap (read @Int . Text.unpack) . Text.words
  where
  go :: [Int] -> V3 Int
  go ns = V3 $ zip (coerce ns) $ repeat mempty
