{-# language TypeApplications #-}
import Control.Monad.State
import Data.Foldable
import Debug.Trace

main :: IO ()
main = do
  xs <- fmap ((\(a : b : _) -> (a, read @Int b)) . words) . lines <$> getContents
  let ((x, y), a) = execState (steps xs) ((0, 0), 0)
  print $ x * y

steps :: [(String, Int)] -> State ((Int, Int), Int) ()
steps = traverse_ step

step :: (String, Int) -> State ((Int, Int), Int) ()
step (s, m) = state $ \p -> ((), f p)
  where
  f ((x, y), a) = traceShowId $ case s of
    "forward" -> ((x + m, y + m * a), a)
    "down"    -> ((x, y), a + m)
    "up"      -> ((x, y), a - m)
    _         -> ((x, y), a)
