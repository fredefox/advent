{-# language LambdaCase #-}
{-# language TypeApplications #-}
import Control.Monad.State
import Data.Foldable
import System.Environment

main :: IO ()
main = do
  (a:b:_) <- fmap (read @Int) <$> getArgs
  let xs = evalState steps (join $ repeat [1..100], 0, a, b)
  traverse_ print $ scanl plus (0, 0, 0) xs

type I = (Int, Int, Int)

plus :: I -> I -> I
plus (_, a0, a1) (n, b0, b1) = (n, a0 + b0, a1 + b1)

type S a = State ([Int], Int, Int, Int) a

roll :: S Int
roll =
  get >>= \case
    (x:xs, n, p1, p2) -> do
      put (xs, succ n, p1, p2)
      pure x
    _ -> undefined

roll3 :: S Int
roll3 = do
  a <- roll
  b <- roll
  c <- roll
  pure $ sum [a,b,c]

step :: S I
step = do
  q1 <- roll3
  q2 <- roll3
  (xs, n, p1, p2) <- get
  let r1 = mod10 $ p1 + q1
  let r2 = mod10 $ p2 + q2
  put (xs, n, r1, r2)
  pure (n, r1, r2)
  
steps :: S [I]
steps = repeatM step

repeatM :: Applicative m => m a -> m [a]
repeatM m = (:) <$> m <*> repeatM m

-- |
-- >>> mod10 <$> [-1,0,1,10]
-- [9,10,1,10]
mod10 :: Int -> Int
mod10 n = succ $ pred n `mod` 10
