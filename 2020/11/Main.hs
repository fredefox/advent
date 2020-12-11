{-# language LambdaCase #-}
import Data.Foldable
import Data.Array
import Control.Monad

main :: IO ()
main = do
  xs <- lines <$> getContents
  let r = mkArray xs
  let ys = untilRepeats $ iterate step r
  traverse_ (putStrLn . unlines . toList') ys
  print $ length $ filter (== '#') $ join $ toList' $ last ys

untilRepeats :: Eq a => [a] -> [a]
untilRepeats = \case
  (x0:x1:xs) -> if x0 == x1 then [x0] else x0 : untilRepeats (x1:xs)
  xs -> xs

toList' :: Array (Int, Int) a -> [[a]]
toList' r = chunksOf (succ m) $ elems r
  where
  (_, (_, m)) = bounds r

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

mkArray :: [[a]] -> Array (Int, Int) a
mkArray xs = listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ case xs of { [] -> 0 ; (x:_) -> length x }

step :: Array (Int, Int) Char -> Array (Int, Int) Char
step r = listArray b $ f <$> indices r
  where
  b = bounds r
  f :: (Int, Int) -> Char
  f idx
    | noOccupied   = '#'
    | becomesEmpty = 'L'
    | otherwise    = y
    where
    y = r ! idx
    ys = (r !) <$> neighbours idx (snd b)
    occupied = filter (== '#') ys
    noOccupied   = y == 'L' && null occupied
    becomesEmpty = y == '#' && (>= 4) (length occupied)

neighbours :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours (i, j) (n, m) = do
  di <- [-1, 0, 1]
  let i' = i + di
  guard $ i' >= 0 && i' <= n
  dj <- [-1, 0, 1]
  let j' = j + dj
  guard $ j' >= 0 && j' <= m
  guard $ i /= i' || j /= j'
  pure (i', j')
