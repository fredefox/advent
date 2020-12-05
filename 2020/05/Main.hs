import Data.Foldable
import Data.List

main :: IO ()
main = do
  let f (a, b) = (toInt a, toInt b)
  let g (a, b) = (a, b, a * 8 + b)
  xs <- sortOn (\(_, _, a) -> a) . fmap (g . f . splitAt 7 . fmap toBool) . lines <$> getContents
  let ((_,_,x):_) = xs
  let (_,_,y) = last xs
  traverse_ print xs
  traverse_ print $ [x..y] \\ fmap (\(_,_,a) -> a) xs

toBool :: Char -> Bool
toBool = \case
  'F' -> False
  'B' -> True
  'L' -> False
  _   -> True

toInt :: [] Bool -> Int
toInt = foldl' go 0
  where
  go acc n = acc * 2 + if n then 1 else 0
