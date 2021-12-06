{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language InstanceSigs #-}
{-# language TypeApplications#-}
import Data.Ix
import Data.Foldable hiding (toList)
import qualified Bag
import GHC.Exts

data Point a = Point a a

deriving stock instance Show a => Show (Point a)
deriving stock instance Eq a => Eq (Point a)
deriving stock instance Ord a => Ord (Point a)

instance Integral a => Ix (Point a) where
  -- Implementation is incorrect for e.g. @range (Point 0 0, Point 2 1)@
  range (Point x0 x1, Point y0 y1)
    | x0 == y0  = Point x0 <$> enumFromTo (min x1 y1) (max x1 y1)
    | otherwise = foldMap g $ enumFromTo (min x0 y0) (max x0 y0)
    where
    g x = if m == 0 then [Point x y] else []
      where
      y = d + x1
      (d, m) = ((x - x0) * (y1 - x1)) `divMod` (y0 - x0)

  index :: (Point a, Point a) -> Point a -> Int
  index = error "undefined"
  inRange :: (Point a, Point a) -> Point a -> Bool
  inRange = error "undefined"

parse :: [a] -> [Point a]
parse (a : b : xs) = Point a b : parse xs
parse _ = []

main :: IO ()
main = do
  xs <- fmap ((\(a:b:_) -> (a, b)) . parse . fmap (read @Int) . words) . lines <$> getContents
  let go ys = print $ length $ filter ((>= 2) . snd) $ toList $ Bag.bag @_ @Int $ foldMap range ys
  go $ filter (\(Point x0 x1, Point y0 y1) -> x0 == y0 || x1 == y1) xs
  go xs
