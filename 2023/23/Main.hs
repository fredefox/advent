{-# language ViewPatterns, MultiWayIf #-}
module Main (main) where

import Data.Foldable hiding (toList)
import Data.Array (Array)
import qualified Data.Array as Array
import Control.Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import GHC.Exts
import GHC.ST (ST)
import qualified GHC.ST as ST
import Data.Array.ST (STArray, Ix)
import qualified Data.Array.ST as ST
import Control.Applicative
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
-- import Debug.Trace

main :: IO ()
main = do
  a <- parse <$> getContents
  let ((s, _):_) = filter (\((i, _), c) -> i == 0 && c == '.') $ Array.assocs a
  let (_, (n, _)) = Array.bounds a
      dn :: (Int, Int) -> Bool
      dn (i, _) = i == n
  -- print s
  -- print $ Array.bounds a
  print $ solve (Array.bounds a) df dn doneVal nx p (neighbors a) s
  where
  df :: T
  df = empty
  doneVal :: T
  doneVal = Just 0
  nx :: T -> T
  nx = fmap succ
  p :: T -> T -> Bool
  p = (<)

type T = Maybe (Max Int)

neighbors :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
neighbors a ix = foldMap go dirs
  where
  dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  (a0, a1) `plus` (b0, b1) = (a0 + b0, a1 + b1)
  go :: (Int, Int) -> [(Int, Int)]
  go d = ix' <$ guard p
    where
    p :: Bool
    p = Array.bounds a `Array.inRange` ix' && q
    q = case (c, d, elem @[] c "<>^v#") of
      (_, _, False) -> True
      ('>', (_, 1), _) -> True
      ('<', (_, -1), _) -> True
      ('v', (1, _), _) -> True
      ('^', (-1, _), _) -> True
      _ -> False
    ix' = ix `plus` d
    c = a Array.! ix'

solve
  :: forall n i
  . Ord i => Show i => Show n
  => Ix i => Monoid n
  -- => (n -> Bool)
  -- -> (n -> n)
  => (i, i) -> n -> (i -> Bool) -> n -> (n -> n) -> (n -> n -> Bool) -> (i -> [i])
  -> i
  -> n
solve bnds df dn doneVal nx p rchbl v0 = ST.runST $ do
  ST.newArray bnds df >>= go
  where
  go :: forall s . STArray s i n -> ST s n
  go warr = step mempty v0
    where
    step :: Set i -> i -> ST s n
    step m i = if
        | i `Set.member` m -> done df
        | dn i -> done doneVal
        | otherwise -> do
           w <- ST.readArray warr i
           w' <- nx <$> foldMap (step (Set.insert i m)) (rchbl i)
           if p w w'
           then pure w'
           else done w
      where
      done :: n -> ST s n
      done v = v <$ ST.writeArray warr i v

parse :: String -> Array (Int, Int) Char
parse = matrix . lines

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs
