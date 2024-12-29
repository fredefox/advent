{-# language GHC2024, PartialTypeSignatures #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec hiding (many)
import Control.Exception hiding (try)
import Data.Char
import Control.Applicative
import qualified Data.List as List

main :: IO ()
main = do
  inp <- getContents
  case runParser parser () mempty inp of
    Left e -> throwIO e
    Right x -> do
      let (keys, locks) = mkKeys x
      print $ length $ filter id $ fit <$> locks <*> keys

mkKeys :: [[String]] -> ([[Int]], [[Int]])
mkKeys xs = foldl' go ([], []) $ fmap mkKey xs
  where
  go (keys, locks) (isLock, x)
    | isLock    = (keys, x:locks)
    | otherwise = (x:keys, locks)

fit :: Ord b => [b] -> [b] -> Bool
fit lock key = and $ zipWith (<=) lock key

mkKey :: [String] -> (Bool, [Int])
mkKey xs = (isLock, fingerprint)
  where
  isLock = case xs of { (('#':_):_) -> True ; _ -> False }
  fingerprint = fmap (go . List.group) $ List.transpose xs
  go (x:_) = pred $ length x
  go _ = 0

parser :: Parsec String () [[String]]
parser = box `sepBy` newline
  where
  line = some (satisfy (not . isSpace)) <* newline
  box = many line
