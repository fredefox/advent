{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
import Text.Read
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Foldable
import Data.Maybe

main :: IO ()
main = do
  (_, xs) <- getInput
  -- traverse_ print $ part1 n xs
  -- print xs
  -- print $ catMaybes $ zipWith (\m n -> fmap (\m -> mkN m n) m) xs [0..]
  let ys = catMaybes $ zipWith (\m n -> fmap (\m -> mkN m n) m) xs [0..]
  traverse_ print $ runN $ mconcat ys
  -- traverse_ print $ runN $ mkN 59 4 <> mkN 7 0 <> mkN 13 1 <> mempty
  -- traverse_ print $ runN $ mconcat [mkN 19 7, mkN 31 6, mkN 59 4, mkN 7 0, mkN 13 1]

part1 :: Int -> [Int] -> [(Int, Int)]
part1 n xs = sortOn (negate . snd) $ go <$> xs
    where
    go :: Int -> (Int, Int)
    go m = (m, (if b == 0 then id else succ) a * m)
      where
      (a, b) = divMod n m

data N = N { runN :: [Int] }

mkN :: Int -> Int -> N
mkN n m = N $ (+ m) <$> iterate (+n) 0

deriving stock instance Show N

instance Semigroup N where
  N xs <> N ys = N $ merge xs ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs@(x:xss) ys@(y:yss) = case x `compare` y of
  LT -> merge xss ys
  EQ -> x : merge xss yss
  GT -> merge xs yss
merge _ _ = mempty

instance Monoid N where
  mempty = mkN 1 0

getInput :: IO (Int, [Maybe Int])
getInput = do
  lns <- fmap Text.lines Text.getContents
  case lns of
    (n : xs : _) -> pure (read @Int $ Text.unpack n, fmap (parseNum @Maybe . Text.unpack) $ Text.splitOn "," xs)
    _ -> fail "Parse error"

parseNum :: Alternative m => String -> m Int
parseNum s = case readMaybe s of
  Nothing -> empty
  Just x -> pure x

-- lcm 3 4 = 12
-- 3 0 -> 0   3   6 9 12
-- 4 1 ->   1   5   9    13
-- f (3 0) (4 1) = 9 < 12 = lcm 3 4

-- k0 * a0 + a1 = k1 * b0 + b1
-- k0 * a0 + (a1 - b1) = k1 * b0
-- (k0 * a0 + (a1 - b1) `div` b0) = k1
meet :: (Int, Int) -> (Int, Int) -> (Int, Int)
meet (a0, a1) (b0, b1) = (k0 * a0 + (a1 - b1)) `div` b0
