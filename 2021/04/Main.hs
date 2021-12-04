{-# language AllowAmbiguousTypes #-}
{-# language LambdaCase #-}
module Main (mark, nat, main) where

import Data.Array.MArray (MArray)
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as Array
import Data.Array.IArray (Array, IArray)
import qualified Data.Array.IArray as Array
import qualified Data.Array.MArray as Array
import GHC.TypeLits
import Data.Kind
import Data.Singletons
import qualified GHC.TypeLits as Nat
import Data.Foldable
import Data.Ix
import qualified Data.List as List

type Board :: Nat -> Nat -> Type
newtype Board n m = Board (IOArray (Integer, Integer) (Int, Int))

empty :: forall n m . KnownNat n => KnownNat m => IO (Board n m)
empty = board (\_ _ -> (0, 0))

board
  :: forall n m
  .  KnownNat n
  => KnownNat m
  => (Integer -> Integer -> (Int, Int))
  -> IO (Board n m)
board f = Board <$> newArray (0, 0) (nat @n, nat @m) (uncurry f)

mark :: forall n m . KnownNat n => KnownNat m => Integer -> Integer -> Board n m -> IO ()
mark i j (Board b) = updateArray b (i, j) (\(v, m) -> (v, succ m))

updateArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e)-> m ()
updateArray a i f = do
  e <- Array.readArray a i
  Array.writeArray a i (f e)

array :: IArray a e => Ix i => i -> i -> (i -> e) -> a i e
array a b f = Array.listArray bounds $ f <$> range bounds
  where
  bounds = (a, b)

newArray :: MArray a e m => Ix i => i -> i -> (i -> e) -> m (a i e)
newArray a b f = Array.newListArray ab $ f <$> range ab
  where
  ab = (a, b)

nat :: forall n . KnownNat n => Integer
nat = Nat.natVal @n Proxy

main :: IO ()
main = do
  xs <- lines <$> getContents
  let (a:bs)
        = filter (not . null . head)
        $ List.groupBy (\a b -> null a == null b) xs
  print $ fmap (read @Int) $ words $ fmap (\case ',' -> ' ' ; x -> x) $ head a
  let bs' :: [[[Int]]]
      bs' = fmap (fmap (fmap (read @Int) . words)) bs
      n = undefined
      m = undefined
  traverse (makeBoard n m) bs'
  traverse_ print bs'

makeBoard :: Int -> Int -> [[Int]] -> IO (Board n m)
makeBoard = undefined

example :: IO ()
example = do
  Board b <- mkBoard
  a <- Array.getAssocs b
  traverse_ print a
  where
  mkBoard :: IO (Board 3 3)
  mkBoard = do
    b <- empty @3 @3
    mark 1 2 b
    pure b

-- foo : (n : Int) -> a -> Vec n a
