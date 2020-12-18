{-# language TypeApplications #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import Data.Foldable

main :: IO ()
main = do
  xs <- fmap (read @Int) . words <$> getContents
  let ys = zip xs [1 :: Int ..]
  let m = Map.fromList $ fmap (fmap pure) ys
  let (a, t) = last ys
  traverse_ print xs
  traverse_ print $ fmap fst $ tail $ stepper (t + 1) (a, m)

stepM :: Ord a => Integral a => a -> State (a, Map a [a]) ()
stepM n = state (pure . step n)

step :: Ord a => Integral a => a -> (a, Map a [a]) -> (a, Map a [a])
step t (a, m) = (a', m')
  where
  xs = fromMaybe [] $ Map.lookup a m
  a' = case xs of
    (a:b:_) -> a - b
    _       -> 0
  m' = Map.insertWith (<>) a' [t] m

stepper :: Ord a => Integral a => a -> (a, Map a [a]) -> [(a, Map a [a])]
stepper t s = s : stepper (succ t) s'
  where
  s' = step t s
