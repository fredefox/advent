{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  xs <- getInput
  let l = Map.toList $ solve1 xs
  traverse_ print l
  print $ sum $ snd <$> l

solve1 :: [(Int, Int, [(Int, Int)])] -> Map Int Int
solve1 xs0 = Map.fromList $ foldMap go xs0
  where
  go :: (Int, Int, [(Int, Int)]) -> [(Int, Int)]
  go (m, v, xs) = fmap (fmap $ mask m v) xs

getInput :: forall a . Bits a => Read a => IO [(a, a, [(a, a)])]
getInput = fmap (f . uncons . Text.lines) . Text.splitOn "\n\n" <$> Text.getContents
  where
  g :: Text -> (a, a)
  g ln = case Text.words ln of (a : b : _) -> (read $ Text.unpack a, read $ Text.unpack b) ; _ -> undefined
  f :: (Text, [Text]) -> (a, a, [(a, a)])
  f (x, xs) = (m, v, g <$> xs)
    where
    go c n = case c of
          'X' -> bit n
          _   -> zeroBits
    go' c n = case c of
          '1' -> bit n
          _   -> zeroBits
    m = buildWith go x
    v = buildWith go' x

buildWith :: Bits a => (Char -> Int -> a) -> Text -> a
buildWith f x = foldl' (.|.) zeroBits $ zipWith f (Text.unpack $ Text.reverse x) [0..]

uncons :: [a] -> (a, [a])
uncons (x:xs) = (x, xs)
uncons _ = error "Empty list"

mask :: Bits a => a -> a -> a -> a
mask m v x = (complement m .&. v) .|. (m .&. x)
