{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language PartialTypeSignatures #-}
{-# language MultiWayIf #-}
{-# options_ghc -Wno-partial-type-signatures #-}
module Main (main, unMatrix) where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Array.ST (STArray)
import qualified Data.Array.ST as ST
import Data.Array.MArray (MArray)
import qualified Data.Array.MArray as MArray
import Control.Monad
import Control.Monad.ST
import Data.Ix (Ix)

main :: IO ()
main = do
  xs <- getContents
  let m = mkMatrix $ lines xs
      ys = iterate step m
  print $ length $ uniqInit ys

uniqInit :: Eq a => [a] -> [a]
uniqInit = \case
  [] -> []
  xs@(x:xss) -> x : fmap snd (takeWhile (uncurry (/=)) $ zip xs xss)

mkMatrix :: [String] -> Array (Int, Int) Char
mkMatrix = \case
  []       -> Array.array ((0, 0), (0, 0)) mempty
  xs@(x:_) -> Array.listArray ((0, 0), (n, m)) (join xs)
    where
    n = pred $ length xs
    m = pred $ length x

unMatrix :: Array (Int, Int) Char -> [String]
unMatrix a = chunksOf (succ m) xs
  where
  xs = Array.elems a
  (_, (_, m)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
  [] -> []
  xs -> a : chunksOf n b
    where
    (a, b) = splitAt n xs

step :: Array (Int, Int) Char -> Array (Int, Int) Char
step a = runST w
  where
  (_, (n', m')) = Array.bounds a
  n = succ n'
  m = succ m'
  w :: forall s . ST s _
  w = do
    (thawA :: STArray s _ _) <- ST.thaw a
    let wr :: ((Int, Int), Char) -> ST s ()
        wr (ix@(i, j), c) = do
          let e = a ! ixr
          if
            | c == '>' && free e -> do
                MArray.writeArray thawA ix  '.'
                MArray.writeArray thawA ixr '>'
            | otherwise -> pure ()
            where
            ixr = (i, succ j `mod` m)
    thawA `foreach` wr
    a0 <- ST.freeze thawA
    let wd :: _ -> ST s ()
        wd (ix@(i, j), c) = do
          let e = a0 ! ixd
          if
            | c == 'v' && free e -> do
                MArray.writeArray thawA ix  '.'
                MArray.writeArray thawA ixd 'v'
            | otherwise -> pure ()
            where
            ixd = (succ i `mod` n, j)
    thawA `foreach` wd
    ST.freeze thawA

foreach :: Ix ix => MArray a e m => a ix e -> ((ix, e) -> m b) -> m ()
foreach a f = do
  xs <- MArray.getAssocs a
  xs `forM_` f

free :: Char -> Bool
free c = c `notElem` ['>', 'v']
