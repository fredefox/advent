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
    Right tbl -> do
      print $ solve [(+), (*)] tbl
      print $ solve [(+), (*), conc] tbl

solve :: (Eq a, Num a) => [a -> a -> a] -> [(a, [a])] -> a
solve ops tbl = sum $ fmap fst $ filter (uncurry (hasSolution ops)) tbl

hasSolution :: Eq a => [a -> a -> a] -> a -> [a] -> Bool
hasSolution ops x = (x `elem`) . combinations ops

combinations :: forall a . [a -> a -> a] -> [a] -> [a]
combinations _ [] = []
combinations ops (x0:xs0) = go [x0] xs0
  where
  go :: [a] -> [a] -> [a]
  go accs [] = accs
  go accs (x:xs) = go (foldMap op ops) xs
    where
    op :: (a -> a -> a) -> [a]
    op f = fmap (`f` x) accs

conc :: Int -> Int -> Int
conc a b = read (show a <> show b)

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
