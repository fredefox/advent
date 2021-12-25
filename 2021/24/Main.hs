{-# language LambdaCase #-}
{-# language TypeApplications #-}
module Main (main) where

import Data.Foldable
import System.Environment

main :: IO ()
main = do
  instructions <- fmap words . lines <$> getContents
  traverse_ print $ map (\x -> (x, valid instructions x)) $ iterate pred 9999999999999999999
-- main = do
--   input <- fmap (read @Int) <$> getArgs
--   let regs = (0, 0, 0, 0)
--   instructions <- fmap words . lines <$> getContents
--   print $ run (regs, input) instructions

valid :: [[String]] -> Integer -> Bool
valid instructions n = z == 0
  where
  ((_, _, _, z), _) = run ((0, 0, 0, 0), fromIntegral <$> input) instructions
  input = digits 10 n

digits :: Integral n => n -> n -> [n]
digits beta = reverse . go
  where
  go n
    | n > 0 = b : go a
    | otherwise = []
    where
    (a, b) = n `divMod` beta

type T = (Int, Int, Int, Int)

run :: (T, [Int]) -> [[String]] -> (T, [Int])
run = foldl' go
  where
  go :: (T, [Int]) -> [String] -> (T, [Int])
  go (regs, xs) = \case
    ["inp", a]    -> (write a (head xs) regs, tail xs)
    ["add", a, b] -> binop a b (+)
    ["mul", a, b] -> binop a b (*)
    ["div", a, b] -> binop a b div
    ["mod", a, b] -> binop a b mod
    ["eql", a, b] -> binop a b eql
    _             -> error "Unknown instruction"
    where
    binop a b f = (write a (readR a regs `f` readV b regs) regs, xs)
    eql :: Eq a => a -> a -> Int
    eql a b = fromEnum $ a == b

write :: String -> Int -> T -> T
write r n (w, x, y, z) = case r of
  "w" -> (n, x, y, z)
  "x" -> (w, n, y, z)
  "y" -> (w, x, n, z)
  "z" -> (w, x, y, n)
  _   -> error "Unknown register"
  
readR :: String -> T -> Int
readR r (w, x, y, z) = case r of
  "w" -> w
  "x" -> x
  "y" -> y
  "z" -> z
  _   -> error "Unknown register"

readV :: String -> T -> Int
readV r (w, x, y, z) = case r of
  "w" -> w
  "x" -> x
  "y" -> y
  "z" -> z
  _   -> read @Int r
