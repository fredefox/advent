import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Foldable

main :: IO ()
main = do
  print $ parse "R32"
  -- getInput >>= traverse_ print
  getInput >>= traverse_ (print . segments)

type K a = (a, a, Bool, a)

segments :: [] (Bool, Int) -> [K Int]
segments xs = State.evalState (traverse go xs) (0, 0)
  where
  go :: (Bool, Int) -> State (Int, Int) (K Int)
  go (b, t) = State.state f
    where
    f :: (Int, Int) -> (K Int, (Int, Int))
    f (n, m) = (k, p)
      where
      k :: K Int
      k = (n, m, b, n)
      p :: (Int, Int)
      p = case b of
        False -> (n    , m + t)
        True  -> (n + t, m    )

sepBy :: Eq a => a -> [a] -> [[a]]
sepBy c xs = case r of
  (_:xss) -> s : sepBy c xss
  _ -> pure s
  where
  (s, r) = span (/= c) xs

parse :: String -> [(Bool, Int)]
parse = fmap go . sepBy ','
  where
  go :: String -> (Bool, Int)
  go [] = error "parse error"
  go (c:xs) = case c of
    'U' -> (False, -n)
    'D' -> (False,  n)
    'L' -> (True, -n)
    'R' -> (True,  n)
    _   -> error "parse error"
    where
    n = read xs
    
getInput :: IO [[(Bool, Int)]]
getInput = fmap parse . lines <$> getContents
