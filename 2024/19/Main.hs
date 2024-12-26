{-# language GHC2021, PartialTypeSignatures, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec hiding (many, optional)
import Data.Functor.Identity
import Control.Exception hiding (try)
import Data.Char
import Control.Applicative
import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad

main :: IO ()
main = do
  inp <- getContents
  case runParser parser () mempty inp of
    Left e -> throwIO e
    Right (candidates, b) -> do
      let xs = fmap (solve @[] candidates) b
      print $ length $ filter (> 0) xs
      print $ sum xs

solve :: forall f . Alternative f => Foldable f => Monad f => f [Char] -> [Char] -> Int
solve candidates xs = last $ Array.elems w
  where
  w :: Array Int Int
  w = Array.listArray bounds (f <$> Array.range bounds)
  u :: Array Int Char
  u = Array.listArray bounds xs
  bounds = (0, length xs)
  f :: Int -> Int
  f ix
    | ix <= 0 = 1
    | otherwise = sum $ do
        candidate <- candidates
        let jx = (ix - length (candidate))
        guard $ jx >= 0
        let x = slice u jx ix
        guard $ x == candidate
        pure $ w Array.! jx

slice :: Enum ix => Ix ix => Array ix a -> ix -> ix -> [a]
slice w a b = (w Array.!) <$> Array.range (a, pred b)

parser :: ParsecT String () Identity ([[Char]], [[Char]])
parser = do
  h <- header
  _ <- newline
  _ <- newline
  (h,) <$> many (many (satisfy isAlpha) <* newline)

header :: ParsecT String u Identity [[Char]]
header
  = many (satisfy isAlpha)
  `sepBy` some (satisfy ((||) <$> (`elem` " ") <*> isPunctuation))
