{-# language TypeApplications #-}
{-# language LambdaCase #-}
import Data.List
import System.Exit
import Control.Monad

main :: IO ()
main = do
  ns <- sort . fmap (read @Int) . lines <$> getContents
  case part1 ns of
    Nothing -> exitFailure
    Just (x, y) -> putStrLn $ show x <> " * " <> show y <> " = " <> show (x * y)
  print $ part2 2020 ns

part1 :: [] Int -> Maybe (Int, Int)
part1 ns = findPair 2020 ns (reverse ns)

findPair :: Int -> [] Int -> [] Int -> Maybe (Int, Int)
findPair _ [] _  = Nothing
findPair _ _  [] = Nothing
findPair m (x0:xs0) (y0:ys0) = go (x0 + y0) (x0, y0) xs0 ys0
  where
  go :: Int -> (Int, Int) -> [] Int -> [] Int -> Maybe (Int, Int)
  go _ _ [] _  = Nothing
  go _ _ _  [] = Nothing
  go n (x1, y1) xs@(x:xss) ys@(y:yss) = case n `compare` m of
    LT -> go (n + x - x1) (x , y1) xss ys
    EQ -> Just (x, y)
    GT -> go (n + y - y1) (x1, y ) xs  yss

unconses :: [] a -> [(a, [] a)]
unconses = \case
  [] -> []
  (x:xs) -> (x, xs) : unconses xs

part2 :: Int -> [] Int -> [] (Int, Int, Int)
part2 n0 ns0 = do
  (n1, ns1 ) <- go (<= n0          ) ns0
  (n2, ns2 ) <- go (<= n0 - n1     ) ns1
  (n3, _ns3) <- go (<= n0 - n1 - n2) ns2
  guard (n1 + n2 + n3 == n0)
  pure (n1, n2, n3)
  where
  go p = unconses . takeWhile p
