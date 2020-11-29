{-# language LambdaCase #-}
{-# language RankNTypes #-}
import Data.Array
import Debug.Trace
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable

main :: IO ()
main = do
  inp <- getContents
  let a = parse inp
  putStrLn $ printMatrix a

chunksOf :: Int -> [] a -> [[a]]
chunksOf n = \case
  [] -> []
  xs -> xs0 : chunksOf n xs1
    where
    (xs0, xs1) = splitAt n xs

printMatrix :: Array (Int, Int) Bool -> String
printMatrix r = unlines $ chunksOf (succ m) $ c <$> elems r
  where
  (_, (_, m)) = bounds r
  c = \case
    False -> '.'
    _     -> '#'

parse :: String -> Array (Int, Int) Bool
parse s = listArray ((0, 0), (n, m)) $ concatMap (fmap c) ls
  where
  ls = lines s
  n = pred $ length ls
  m = pred $ length $ head ls
  c = \case
    '.' -> False
    _   -> True

run :: Array (Int, Int) Bool -> Array (Int, Int) Bool
run r = step (thaw r)

step :: (forall s . ST s (STArray s (Int, Int) Bool)) -> Array (Int, Int) Bool
step getR = runSTArray $ do
  r <- getR
  getAssocs r >>= traverse_ (\((i, j), b) -> do { _ })
  pure r
