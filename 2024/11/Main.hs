{-# options_ghc -Wall #-}
module Main (main) where

import Data.Foldable hiding (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GHC.IsList
import System.IO

main :: IO ()
main = do
  xs <- fmap (read @Int) . words <$> getContents
  hSetBuffering stdout LineBuffering
  traverse_ (print . sum) $ iterate stepMap $ IntMap.fromList $ fmap (,1) xs

stepMap :: IntMap Int -> IntMap Int
stepMap = IntMap.fromListWith (+) . foldMap go . toList
  where
  go (k, n) = fmap (,n) $ expand k

expand :: Int -> [Int]
expand 0 = [1]
expand n | m == 0 = [read (take d sn), read (drop d sn)]
  where
  sn = show n
  (d, m) = length sn `divMod` 2
expand n = [n * 2024]
