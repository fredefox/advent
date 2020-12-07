main :: IO ()
main = do
  ns <- getInput
  print $ sum $ fmap fuel  ns
  print $ sum $ fmap fuel2 ns

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input"

fuel :: Int -> Int
fuel n = (n `div` 3) - 2

fuel2 :: Int -> Int
fuel2 = go 0
  where
  go acc n = if m <= 0 then acc else go (m + acc) m
    where
    m = fuel n
