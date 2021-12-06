{-# language TypeApplications #-}
import Data.Foldable (traverse_)
import GHC.Exts
import Bag (Bag)
import qualified Bag

main :: IO ()
main = do
  xs <- fmap (read @Int) . words <$> getContents
  traverse_ (print . Bag.size) $ iterate step $ toBag xs

toBag :: [Int] -> Bag Int Int
toBag = mconcat . fmap (`Bag.singleton` 1)

step :: Bag Int Int -> Bag Int Int
step xs = Bag.map go xs <> Bag.singleton 8 n
  where
  go 0 = 6
  go x = pred x
  n = Bag.size $ Bag.filter (== 0) xs
