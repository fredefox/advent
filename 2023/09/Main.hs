{-# language LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main) where

main :: IO ()
main = do
  xs <- fmap (fmap (read @Int) . words) . lines <$> getContents
  print $ sum $ one <$> xs

one :: Integral n => [n] -> n
one x = sum $ last <$> diffs x

diffs :: Integral n => [n] -> [[n]]
diffs = f . span (any (/= 0)) . go
  where
  go = \case
    xs@(_:xss) -> xs : (go $ zipWith (-) xss xs)
    [] -> []
  f (xs, []) = xs
  f (xs, y:_) = xs <> [y]
