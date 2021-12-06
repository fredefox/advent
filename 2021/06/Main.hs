{-# language TypeApplications #-}
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Exts

main :: IO ()
main = do
  xs <- fmap (read @Int) . words <$> getContents
  traverse_ (print . sum) $ iterate step (toBag xs)

toBag :: [Int] -> Map Int Int
toBag = Map.fromListWith (+) . fmap go
  where
  go x = (x, 1)

step :: Map Int Int -> Map Int Int
step xs = Map.unionWith (+) (Map.mapKeysWith (+) go xs) (Map.singleton 8 n)
  where
  go 0 = 6
  go x = pred x
  n = sum $ fmap snd $ filter ((== 0) . fst) $ toList xs
