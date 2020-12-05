{-# language LambdaCase #-}
{-# language TypeApplications #-}
import Data.Foldable
import Data.List
import System.Environment
import Control.Monad
import qualified Codec.BMP as BMP
import qualified Data.ByteString as ByteString

main :: IO ()
main = do
  (n, m) <- \case { (n:m:_) -> (read @Int n, read @Int m) ; [] -> (0, 0) } <$> getArgs
  xs <- getInput (n * m)
  -- print xs
  -- pt1 xs
  putStrLn $ unlines $ fmap (join . fmap p) $ chunksOf n $ solve xs
  -- BMP.writeBMP "out.bmp" $ BMP.packRGBA32ToBMP n m $ ByteString.pack $ (toEnum <$> solve xs) <> repeat 0
   where
   p = \case
     0 -> " "
     _ -> "#"

pt1 :: [[Int]] -> IO ()
pt1 = traverse_ print . fmap score . sortOn (length . filter (== 0))

score :: [] Int -> Int
score xs = p 1 xs * p 2 xs
  where
  p n = length . filter (== n)

getInput :: Int -> IO [[Int]]
getInput n = parse n . join . lines <$> getContents

parse :: Int -> String -> [[Int]]
parse n = fmap (fmap (read . pure)) . chunksOf n

chunksOf :: Int -> [] a -> [[a]]
chunksOf n = \case
  [] -> []
  xs -> a : chunksOf n b
    where
    (a, b) = splitAt n xs

solve :: [[Int]] -> [Int]
solve = foldr (zipWith merge) $ repeat 2

merge :: Int -> Int -> Int
merge 2 n = n
merge n 2 = n
merge n _ = n
