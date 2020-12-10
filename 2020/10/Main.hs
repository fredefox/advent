{-# language ScopedTypeVariables #-}
import Data.List
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  xs' <- sort . fmap read . lines <$> getContents
  let xs = xs' <> [last xs' + 3]
  traverse_ print $ fmap length $ group $ sort $ solve xs
  let xs'' = 0 : xs
  print $ pathsC (minimum xs'') (maximum xs'') $ adjecency xs''

solve :: [Int] -> [Int]
solve xs = fmap fst $ tail $ scanl (\(_acc, a0) a1 -> (a1 - a0, a1)) (0, 0) $ sort xs

adjecency :: Integral n => [n] -> Map n [n]
adjecency xs = Map.fromListWith (<>) $ zipWith (\x xs' -> (x, takeWhile (<= x + 3) $ tail xs')) xs $ tails xs

pathsC:: forall n . Integral n => Ord n => n -> n -> Map n [n] -> n
pathsC s0 t m = go s0 mempty
  where
  go :: n -> Map n n -> n
  go s visited
    | s == t    = 1
    | otherwise = case Map.lookup s visited of
        Nothing -> case Map.lookup s m of
          Nothing -> 0
          Just xs -> snd $ foldr go' (visited, 0) xs
            where
            go' :: n -> (Map n n, n) -> (Map n n, n)
            go' x (m', acc) = let k = go x m' in (Map.insert x k m', k + acc)
        Just res -> res
