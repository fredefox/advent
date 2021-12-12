{-# language MultiWayIf #-}
{-# language TypeApplications #-}
import Data.Foldable
import Data.Char
import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Array.MArray as MArray
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray
import Control.Monad
import Data.Ix

main :: IO ()
main = do
  xs <- fmap (fmap digitToInt) . lines <$> getContents
  ma <- MArray.thaw $ matrix xs
  -- printMatrix ma
  run ma

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ head xs

step :: IOArray (Int, Int) Int -> IO ()
step a = do
  incr a
  flashes a

incr :: IOArray (Int, Int) Int -> IO ()
incr a = a `foreach_` f
  where
  f :: (Int, Int) -> Int -> IO ()
  f ix x = IOArray.writeArray a ix (succ x)

flashes :: IOArray (Int, Int) Int -> IO ()
flashes a = do
  while (flash a)
  -- a `foreach_` reset
  -- where
  -- reset :: (Int, Int) -> Int -> IO ()
  -- reset ix n = if
  --   | n > 9 -> IOArray.writeArray a ix 0
  --   | otherwise -> pure ()

flash :: IOArray (Int, Int) Int -> IO Bool
flash a = fmap or $ a `foreach` f
  where
  f :: (Int, Int) -> Int -> IO Bool
  f ix n = if
    | n > 9     -> do
        r <- fmap or $ traverse g (neighbors ix)
        IOArray.writeArray a ix 0
        pure r
    | otherwise -> pure False
    where
    g :: (Int, Int) -> IO Bool
    g ix' = do
      b <- IOArray.getBounds a
      if
        | b `inRange` ix' -> do
          n' <- succ <$> IOArray.readArray a ix'
          IOArray.writeArray a ix' n'
          pure $ n' > 9
        | otherwise -> pure False

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = (,) <$> [pred x..succ x] <*> [pred y..succ y]

foreach :: Ix i => IOArray i e -> (i -> e -> IO a) -> IO [a]
foreach a f = do
  xs <- MArray.getAssocs a
  traverse (uncurry f) xs

foreach_ :: Ix i => IOArray i e -> (i -> e -> IO a) -> IO ()
foreach_ a f = void $ a `foreach` f

while :: IO Bool -> IO ()
while m = do
  b <- m
  when b $ while m

iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = do
  a' <- f a
  iterateM f a'

run :: IOArray (Int, Int) Int -> IO ()
run a = go 0 0
  where
  go i n = do
    putStrLn $ "After step " <> show i <> ":"
    m <- count a
    let n' = n + m
    printMatrix a
    putStrLn ""
    step a
    go (succ i) n'

count :: Ix i => IOArray i Int -> IO Int
count a = sum <$> (a `foreach` f)
  where
  f _ e = pure $ if e == 0 then 1 else 0

printMatrix :: IOArray (Int, Int) Int -> IO ()
printMatrix a = do
  (_, (n, _)) <- IOArray.getBounds a
  xs <- IOArray.getElems a
  putStr $ unlines $ fmap (fmap f) $ chunksOf (succ n) xs
  where
  f = intToDigit
  -- f 0 = ' '
  -- f n = intToDigit n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs
