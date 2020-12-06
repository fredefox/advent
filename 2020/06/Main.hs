{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language LambdaCase #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (fromList)

main :: IO ()
main = do
  xs <- getInput
  print $ sum $ fmap solve1 xs
  print $ sum $ fmap solve2 xs

getInput :: IO [[Text]]
getInput = fmap Text.words . Text.splitOn "\n\n" <$> Text.getContents

solve1 :: [Text] -> Int
solve1 = Set.size . foldMap (fromList . Text.unpack)

solve2 :: [Text] -> Int
solve2 = Set.size . intersections . fmap (fromList @(Set _) . Text.unpack)

intersections :: Ord a => [Set a] -> Set a
intersections = \case
  [] -> Set.empty
  xs@(_:_) -> foldr1 Set.intersection xs
