{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
import Text.Read
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Foldable

main :: IO ()
main = do
  (n, xs) <- getInput
  traverse_ print $ part1 n xs

part1 :: Int -> [Int] -> [(Int, Int)]
part1 n xs = sortOn (negate . snd) $ go <$> xs
    where
    go :: Int -> (Int, Int)
    go m = (m, (if b == 0 then id else succ) a * m)
      where
      (a, b) = divMod n m


getInput :: IO (Int, [] Int)
getInput = do
  lns <- fmap Text.lines Text.getContents
  case lns of
    (n : xs : _) -> pure (read @Int $ Text.unpack n, foldMap (parseNum @[] . Text.unpack) $ Text.splitOn "," xs)
    _ -> fail "Parse error"

parseNum :: Alternative m => String -> m Int
parseNum s = case readMaybe s of
  Nothing -> empty
  Just x -> pure x
