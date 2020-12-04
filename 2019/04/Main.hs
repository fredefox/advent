{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
import Data.Foldable
import System.Environment
import Data.Coerce
import Data.Monoid
import Data.List

main :: IO ()
main = do
  args <- fmap read <$> getArgs
  let xs = case args of
             [] -> enumFrom 0
             [x] -> enumFrom x
             (x:y:_) -> enumFromTo x y
  traverse_ print $ filter isValid xs

isValid :: Int -> Bool
isValid = p . toDigits @Int 10
  where
  p :: [Int] -> Bool
  p = coerce @_ @([[Int] -> Bool] -> [Int] -> Bool) (mconcat @([Int] -> All)) [p0, p1, p2]
  p0 = (== 6) . length
  -- p1 xs = or  $ zipWith (==) xs $ tail xs
  p1 = any ((== 2) . length) . group
  p2 xs = and $ zipWith (<=) xs $ tail xs

toDigits :: forall n . Integral n => Eq n => n -> n -> [] n
toDigits beta = go []
  where
  go :: [] n -> n -> [] n
  go acc n
    | n == 0 = acc
    | otherwise = go (b : acc) a
      where
      (a, b) = n `divMod` beta
