{-# language TypeApplications #-}
{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language OverloadedLists #-}
{-# language MultiWayIf #-}
{-# language DeriveFoldable #-}
module Main (main, Tree(..)) where

import Text.ParserCombinators.ReadPrec
import Control.Monad
import Text.Read
import Control.Applicative
import Data.Foldable
import qualified Data.Foldable as Foldable
import Data.Coerce
import Data.Monoid
import Data.List (intercalate)
import GHC.Exts
import qualified Data.List
import Data.Ix (Ix)
import Data.Array (Array)
import qualified Data.Array as Array

main :: IO ()
main = do
  -- xs <- getContents
  -- let trees = read @(Tree Int) <$> lines xs
  -- traverse_ print trees
  let
    t :: Tree Int
    t = [1, 2] <> [[3, 4], 5]
  print t
  print $ Array.elems $ toArray $ depths t

data Tree a = Leaf a | Node [Tree a] deriving (Eq, Ord, Foldable)

instance Num a => Num (Tree a) where
  fromInteger = Leaf . fromInteger

instance IsList (Tree a) where
  type Item (Tree a) = Tree a
  fromList = Node
  toList = \case
    Leaf n -> [Leaf n]
    Node xs -> xs

instance Read a => Read (Tree a) where
  readPrec :: ReadPrec (Tree a)
  readPrec = node <|> leaf

instance Show a => Show (Tree a) where
  show = \case
    Leaf x -> show x
    Node xs -> "(" <> intercalate "," (show <$> xs) <> ")"

instance Integral a => Semigroup (Tree a) where
  t0 <> t1 = reduce $ Node [t0, t1]

reduce :: forall a . Integral a => Tree a -> Tree a
reduce t = _
  where
  a = toArray $ depths t
  go :: Array Int (Int, a) -> Tree a
  go a = _ $ Array.assocs a

depths :: Tree a -> Tree (Int, a)
depths = go 0
  where
  go n = \case
    Leaf a -> Leaf (n, a)
    Node xs -> Node $ go (succ n) <$> xs

toArray :: Foldable t => t a -> Array Int a
toArray = listArray . Foldable.toList

listArray :: [a] -> Array Int a
listArray xs = Array.listArray (0, pred $ length xs) xs

fromArray :: forall a . Array Int a -> Tree a
fromArray = Node . go 0 . Array.assocs
  where
  go :: Int -> [(Int, a)] -> [Tree a]
  go n = fmap f
    where
    f :: (Int, a) -> Tree a
    f (m, a)
      | n == m    = _
      | otherwise = _
  

(.>) :: [a] -> Int -> Bool
xs .> n = not $ null $ drop n xs

instance Integral a => Monoid (Tree a) where
  mempty = Node mempty

leaf :: Read a => ReadPrec (Tree a)
leaf = Leaf <$> readPrec

node :: Read a => ReadPrec (Tree a)
node = Node <$> brackets open (sepBy comma readPrec) close

brackets :: ReadPrec w0 -> ReadPrec a -> ReadPrec w1 -> ReadPrec a
brackets w0 w1 w2 = do
  _ <- w0
  a <- w1
  _ <- w2
  pure a

open :: ReadPrec ()
open = satisfy (`elem` (['(', '[', '{'] :: String))

close :: ReadPrec ()
close = satisfy (`elem` ([')', ']', '}'] :: String))

satisfy :: (Char -> Bool) -> ReadPrec ()
satisfy p = do
  c <- get
  unless (p c) pfail

sepBy :: ReadPrec w -> ReadPrec a -> ReadPrec [a]
sepBy d w = do
  as  <- many $ w <* d
  a <- w
  pure $ as <> [a]

char :: Char -> ReadPrec ()
char c = satisfy (== c)

comma :: ReadPrec ()
comma = char ','
