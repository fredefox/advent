{-# language OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable

main :: IO ()
main = Text.getContents >>= traverse_ print . fmap go . Text.lines
  where
  go :: Text -> (Text, [Text])
  go s = case Text.splitOn "contain" s of
    [] -> (mempty, mempty)
    (x:xs) -> (Text.strip x, Text.strip <$> (Text.splitOn "," $ Text.intercalate "contain" xs))
