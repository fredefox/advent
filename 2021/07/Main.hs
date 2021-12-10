{-# language TypeApplications #-}
import qualified Data.List

main :: IO ()
main = do
  xs <- Data.List.sort . fmap (read @Int) . words <$> getContents
  print $ value (middle xs) xs
  print $ minimum $ (`value2` xs) <$> [minimum xs..maximum xs]

middle :: [a] -> a
middle xs = xs !! n
  where
  n = length xs `div` 2

value :: Int -> [Int] -> Int
value x = sum . fmap delta
  where
  delta y = abs (x - y)

value2 :: Int -> [Int] -> Int
value2 x = sum . fmap delta
  where
  delta y = (n * succ n) `div` 2
    where
    n = abs (x - y)
