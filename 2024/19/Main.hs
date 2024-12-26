{-# language GHC2021, PartialTypeSignatures, LambdaCase #-}
-- {-# options_ghc -Wall #-}
module Main (main) where

import Text.Parsec hiding (many, optional)
import Data.Functor.Identity
import Control.Exception hiding (try)
import Data.Char
import Control.Applicative
import System.Environment
import Text.Read
import qualified Data.Map as Map
import Data.Foldable
import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad
import qualified Data.List as List

main :: IO ()
main = do
  inp <- getContents
  case runParser parser () mempty inp of
    Left e -> throwIO e
    Right (candidates, b) -> do
      let xs = filter (not . null . solve candidates) b
      traverse_ putStrLn xs
      print $ length xs

solve :: forall f . Alternative f => Monad f => f [Char] -> [Char] -> f [String]
solve candidates xs = last $ Array.elems w
  where
  w :: Array Int (f [String])
  w = Array.listArray bounds (f <$> Array.range bounds)
  u :: Array Int Char
  u = Array.listArray bounds xs
  bounds = (0, length xs)
  f :: Int -> f [String]
  f ix
    | ix <= 0 = pure []
    | otherwise = do
        candidate <- candidates
        let jx = (ix - length (candidate))
        guard $ jx >= 0
        let x = slice u jx ix
        guard $ x == candidate
        (candidate:) <$> w Array.! jx

-- 398 is too high
slice :: Enum ix => Ix ix => Array ix a -> ix -> ix -> [a]
slice w a b = (w Array.!) <$> Array.range (a, pred b)

split :: [Char] -> [Char] -> [[Char]]
split x y
  | x `List.isPrefixOf` y = pure $ drop (length x) y
  | otherwise = []

parser :: ParsecT String () Identity ([[Char]], [[Char]])
parser = do
  h <- header
  newline
  newline
  (h,) <$> many (many (satisfy isAlpha) <* newline)

header :: ParsecT String u Identity [[Char]]
header
  = many (satisfy isAlpha)
  `sepBy` some (satisfy ((||) <$> (`elem` " ") <*> isPunctuation))
