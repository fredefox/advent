{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative

main :: IO ()
main = do
  m <- getInput
  print $ solve m "COM"
  print $ distance m "YOU" "SAN" "COM"

getInput :: IO (Map Text [Text])
getInput = Map.unionsWith (<>) . fmap parse . Text.lines <$> Text.getContents

parse :: Text -> Map Text [Text]
parse txt = case Text.splitOn ")" txt of
  [a]         -> Map.singleton a mempty
  (a : bs)    -> Map.singleton a [Text.intercalate ")" bs]
  _           -> Map.singleton mempty mempty
  
solve :: Map Text [Text] -> Text -> Int
solve m = go 0
  where
  go :: Int -> Text -> Int
  go acc s = case Map.lookup s m of
    Nothing -> acc
    Just xs  -> acc + sum (go (succ acc) <$> xs)

-- Just for fun:
-- topsort :: forall a . Ord a => Map a ([] a) -> a -> [] a
-- topsort m = go mempty
--   where
--   go :: [] a -> a -> [] a
--   go as a = a : case Map.lookup a m of
--     Nothing -> as
--     Just xs -> foldMap (go as) xs

distance :: Map Text [Text] -> Text -> Text -> Text -> Maybe Int
distance m a b = uncurry (liftA2 (+)) . go Nothing Nothing
  where
  go :: Maybe Int -> Maybe Int -> Text -> (Maybe Int, Maybe Int)
  go i j k
    | k == a = (Just 0, j)
    | k == b = (i, Just 0)
    | otherwise = case Map.lookup k m of
      Nothing -> (i, j)
      Just xs -> case foldr (\(a0, a1) (b0, b1) -> (mm a0 b0, mm a1 b1)) (i, j) $ fmap (go i j) xs of
        (Nothing, b0) -> (Nothing, succ <$> b0)
        (a0, Nothing) -> (succ <$> a0, Nothing)
        res@(Just{}, Just{}) -> res

mm :: Ord a => Maybe a -> Maybe a -> Maybe a
mm Nothing b = b
mm a Nothing = a
mm a b = min <$> a <*> b
