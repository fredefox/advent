{-# language DerivingStrategies #-}
{-# language StandaloneDeriving #-}
{-# language NamedFieldPuns #-}
import Data.Foldable
import Data.Monoid
import Control.Monad.State
import Data.Coerce
import Debug.Trace

main :: IO ()
main = do
  xs <- fmap parse . lines <$> getContents
  -- traverse_ print xs
  -- print $ runState (step ('F', 10)) $ S (V2 1 0) mempty
  putStrLn $ ppr $ run xs

ppr :: S -> String
ppr (S (V2 (Sum a) (Sum b)) (V2 (Sum c) (Sum d))) = unwords [show a, show b, "|", show c, show d]

run :: [(Char, Int)] -> S
run xs = execState (traverse_ step xs) $ S (V2 0 0) (V2 10 (-1))

parse :: String -> (Char, Int)
parse [] = error "Parse error"
parse (x:xs) = (x, read xs)

data V2 a = V2 a a

instance Semigroup a => Semigroup (V2 a) where
  V2 a0 a1 <> V2 b0 b1 = V2 (a0 <> b0) (a1 <> b1)

instance Monoid a => Monoid (V2 a) where
  mempty = V2 mempty mempty

deriving stock instance Show a => Show (V2 a)

data S = S
  { position  :: V2 (Sum Int)
  , waypoint  :: V2 (Sum Int)
  }

deriving stock instance Show S

type Ship a = State S ()

rotate :: Num a => V2 a -> V2 a -> V2 a
rotate (V2 a0 a1) (V2 b0 b1) = V2 (a0 * b0 - (a1 * b1)) (a0 * b1 + a1 * b0)

scale :: Num a => a -> V2 a -> V2 a
scale n (V2 a0 a1) = V2 (a0 * n) (a1 * n)

step :: (Char, Int) -> Ship ()
step (c, n) = state f
  where
  f :: S -> ((), S)
  f S{ position, waypoint } = ((), let x = S position' waypoint' in trace (show (c, n) <> " " <> ppr x) x )
    where
    -- direction' :: V2 (Sum Int)
    -- direction' = case c of
    --   'R' -> rotate direction $ rotation quarterTurns
    --   'L' -> rotate direction $ rotation $ negate quarterTurns
    --   _   -> direction
    quarterTurns = n `div` 90
    rotation m = case m `mod` 4 of
      0 -> V2 1 0
      1 -> V2 0 1
      2 -> V2 (-1) 0
      _ -> V2 0 (-1)
    position' :: V2 (Sum Int)
    position' = position <> delta
    delta :: V2 (Sum Int)
    delta = coerce $ case c of
      'F' -> scale n $ coerce waypoint
      _   -> V2 0 0
    waypoint' :: V2 (Sum Int)
    waypoint' = rotate (waypoint <> deltaWaypoint) $ rotation $ case c of
      'R' -> quarterTurns
      _ -> negate quarterTurns
    deltaWaypoint = coerce $ case c of
      'N' -> V2 0 (negate n)
      'S' -> V2 0 n
      'W' -> V2 (negate n) 0
      'E' -> V2 n 0
      _   -> V2 0 0
