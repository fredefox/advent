{-# language LambdaCase #-}
import Data.Foldable

main :: IO ()
main = do
  ls <- lines <$> getContents
  let res = uncurry (runIt ls) <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print res
  print $ product res

runIt :: [String] -> Int -> Int -> Int
runIt ls n m =
  let ks = fmap go $ solve n $ skipping m ls
  -- putStrLn $ unlines $ solve (negate n) ks
  in sum $ fmap cnt ks
  where
  go = \case
    [] -> []
    ('.':xs) -> 'O':xs
    ('#':xs) -> 'X':xs
    xs       -> xs
  cnt :: String -> Int
  cnt = \case
    []      -> 0
    ('O':_) -> 0
    ('X':_) -> 1
    _       -> 0

skipping :: Int -> [] a -> [] a
skipping n xs = fmap fst $ filter p $ zip xs [0..]
  where
  p (_, i) = i `mod` n == 0

-- main = do
--   inp <- getInput
--   let rot = solve 3 inp
--   putStrLn $ showGrid rot
--   putStrLn $ visit $ unlines $ fmap (fmap tree) rot
--   print $ length $ filter id $ fmap head rot

visit :: String -> String
visit = unlines . k . fmap go . lines
  where
  go :: String -> String
  go = \case
    [] -> []
    (x:xs) -> f x : xs
  f = \case
    '#' -> 'X'
    _   -> 'O'
  k = rotate (-3)

showGrid :: [[Bool]] -> String
showGrid = unlines . fmap (foldMap (pure . tree))

tree :: Bool -> Char
tree = \case
  False -> '.'
  True  -> '#'

solve :: Int -> [[a]] -> [[a]]
solve n xs = zipWith (\xss i -> rotate (i * n) xss) xs [0..]

rotate :: Int -> [a] -> [a]
rotate n xs = b <> a
  where
  (a, b) = splitAt (n `mod` length xs) xs

getInput :: IO [[Bool]]
getInput = fmap (fmap go) . lines <$> getContents
  where
  go :: Char -> Bool
  go = \case
    '.' -> False
    _   -> True
