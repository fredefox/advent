{-# language TypeApplications #-}
import Data.Foldable
import Data.Sequence (Seq(..), ViewL(..))
import qualified Data.Sequence as Seq
import GHC.Exts (fromList)
import System.Environment
import Data.List

main :: IO ()
main = do
  xs <- fmap (read @Int) . lines <$> getContents
  (n:_) <- fmap (read @Int) <$> getArgs
  case verify n xs of
    Nothing -> pure ()
    Just r -> do
      print r
      traverse_ print $ filter (\(s,_,_,_) -> s == r) $ (\x -> (sum x, x, minimum x, maximum x)) <$> sublistSums r xs

verify :: Int -> [] Int -> Maybe Int
verify b xs = case splitAt b xs of
  (_, []) -> Nothing
  (xs', ys) -> verif s ys
    where
    s = fromList @(Seq _) $ xs'

verif :: Seq Int -> [Int] -> Maybe Int
verif _ [] = Nothing
verif s (x:xs)
  | cond = verif (shift x s) xs
  | otherwise = Just x
  where
  cond :: Bool
  cond = or @[] $ do
    (a, s') <- toList $ viewls s
    b <- toList s'
    pure (a + b == x)

viewls :: Seq a -> Seq (a, Seq a)
viewls s = case Seq.viewl s of
  (a :< as) -> (a, as) :<| viewls as
  _ -> mempty

shift :: a -> Seq a -> Seq a
shift a as = ass :|> a
  where
  (_ :< ass) = Seq.viewl as

sublistSum :: Int -> [Int] -> [Int]
sublistSum n xs = fmap fst $ takeWhile p $ zip xs $ scanl1 (+) xs
  where
  p :: (Int, Int) -> Bool
  p = (<= n) . snd

sublistSums :: Int -> [Int] -> [[Int]]
sublistSums n = fmap (sublistSum n) . tails

sublistP :: Semigroup n => (n -> Bool) -> [n] -> [n]
sublistP p xs = fmap fst $ takeWhile (p . snd) $ zip xs $ scanl1 (<>) xs
