{-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec
import Data.Functor.Identity
import Control.Exception

main :: IO ()
main = do
  xs <- getContents
  case runParser parser () mempty xs of
    Left e -> throwIO e
    Right tbl -> print $ sum $ fmap fst $ filter (uncurry solve) tbl

solve :: Int -> [Int] -> Bool
solve x = (x `elem`) . combinations

combinations :: [Int] -> [Int]
combinations [] = []
combinations (x0:xs0) = go [x0] xs0
  where
  go :: [Int] -> [Int] -> [Int]
  go accs [] = accs
  go accs (x:xs) = go (fmap (x +) accs <> fmap (x *) accs) xs

parser :: Parsec String () [(Int, [Int])]
parser = many $ line <* newline

line :: Parsec String () (Int, [Int])
line = do
  n <- number
  _ <- char ':'
  _ <- many space
  ns <- number `sepBy` spaces'
  pure $ (n, ns)

spaces' :: Parsec String u [Char]
spaces' = many (char ' ')

number :: Stream s Identity Char => Parsec s u Int
number = read <$> many1 digit
