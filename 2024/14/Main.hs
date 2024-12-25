{-# language GHC2021, PartialTypeSignatures, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec hiding (many, optional)
import Data.Functor.Identity
import Control.Exception hiding (try)
import Data.Char
import Control.Applicative
import System.Environment
import Text.Read
import qualified Data.Map as Map

main :: IO ()
main = do
  xs <- getContents
  (n, m, k) <- pArgs <$> getArgs
  case runParser parser () mempty xs of
    Left e -> throwIO e
    Right p -> do
      let (pos, deltas) = unzip p
      let final = (!! k) $ iterate (moves n m deltas) pos
      print $ product $ Map.elems $ countEm n m final


countEm :: Int -> Int -> [(Int, Int)] -> (Map.Map (Ordering, Ordering) Integer)
countEm n m xs = Map.filterWithKey p $ Map.fromListWith (+) $ (\x -> (cls n m x, 1)) <$> xs
  where
  p (EQ, _) _ = False
  p (_, EQ) _ = False
  p _ _ = True

cls :: Int -> Int -> (Int, Int) -> (Ordering, Ordering)
cls n m (x, y) = (classify n x, classify m y)

classify :: Int -> Int -> Ordering
classify n x = x `compare` (n `div` 2)

pArgs :: [String] -> (Int, Int, Int)
pArgs (sn:sm:sk:_) | (Just n, Just m, Just k) <- (readMaybe sn, readMaybe sm, readMaybe sk) = (n, m, k)
pArgs _ = (10, 6, 100)

skipTill :: Stream s m Char => ParsecT s u m b -> ParsecT s u m a -> ParsecT s u m a
skipTill p q = try (manyTill p (lookAhead q)) *> q

number :: Parsec String () Int
number = fmap read $ some $ satisfy ((||) <$> isDigit <*> (== '-'))

move :: Int -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
move n m (dx, dy) (x, y) = ((x + dx) `mod` n, (y + dy) `mod` m)

moves :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
moves n m = zipWith (move n m)

parser :: ParsecT String () Identity [R Int]
parser = many $ line <* some newline

type R n = ((n, n), (n, n))

line :: ParsecT String () Identity (R Int)
line = (,) <$> skipPair <*> skipPair

skipPair :: ParsecT String () Identity (Int, Int)
skipPair = skipTill anyChar pair

pair :: ParsecT String () Identity (Int, Int)
pair = do
  n <- number
  _ <- char ','
  m <- number
  pure (n, m)
