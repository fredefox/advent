{-# language TypeApplications #-}
import Data.Foldable
import System.Environment
import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.Ix as Ix

main :: IO ()
main = do
  (p0:p1:n:_) <- fmap (read @Int) <$> getArgs
  let three = [1,2,3]
      rolls :: [Int]
      rolls = do
        a <- three
        b <- three
        c <- three
        pure $ sum [a, b, c]
  print $ uncurry max $ solveA rolls n (p0, 0, p1, 0)

-- | Player positions and scores.
type K = (Int, Int, Int, Int)

-- | Maps player positions, current positions and scores to number of
-- wins.
type A = Array K U

solveA :: [Int] -> Int -> K -> U
solveA rolls n k = arr ! k
  where
  arr :: A
  arr = Array.listArray bounds $ f <$> Ix.range bounds
  bounds = ((1, 0, 1, 0), (10, n, 10, n))
  f :: K -> U
  f (p0, s0, p1, s1) = foldl' plus (0, 0) $ g <$> rolls
    where
    g :: Int -> U
    g roll
      | s0' >= n  = (1, 0)
      | otherwise = swap $ arr ! (p1, s1, p0', s0')
        where
        p0' = mod10 $ roll + p0
        s0' = p0' + s0

type U = (Int, Int)

plus :: U -> U -> U
plus (a0, a1) (b0, b1) = (a0 + b0, a1 + b1)

swap :: U -> U
swap (a, b) = (b, a)

mod10 :: Int -> Int
mod10 n = succ $ pred n `mod` 10
