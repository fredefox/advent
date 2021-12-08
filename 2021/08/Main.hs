{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  xs <- fmap ((\case (x0:x1:_) -> (x0, x1)) . fmap Text.words . Text.splitOn "|") . Text.lines <$> Text.getContents
  -- traverse_ print xs
  -- traverse_ print $ filter ((`elem` [2, 3, 4, 7]). length) $ foldMap snd xs
  print $ length $ filter ((`elem` [2, 3, 4, 7]). length) $ foldMap (fmap Text.unpack . snd) xs
  -- traverse_ (print . fmap digit . snd) xs
  -- let x0 = head xs
  let x0 = (mempty, ["fdgacbe", "gcbe"])
  print $ (`execState` mempty) $ traverse_ record $ uncurry (<>) x0

-- | Number of segments to digit
digit :: Text -> Int
digit s = case lookup (Text.length s) m of { Just n -> n }
  where
  m :: [(Int, Int)]
  m =
    [ 6 |> 0
    , 2 |> 1
    , 5 |> 2
    , 5 |> 3
    , 4 |> 4
    , 5 |> 5
    , 6 |> 6
    , 3 |> 7
    , 7 |> 8
    , 6 |> 9
    ]
    where
    (|>) = (,)

-- | Possible segments that a wire connects to.
type E = Map Char (Set Char)

-- | Record possible connections. If we e.g. see @ab@ we know that
-- @
--    a -> c f
--    b -> c f
-- @
-- because @ab@ uniquely identified the digit one which is displayed
-- on segments @c@ and @f@. Note that we're not recording that the
-- mappings @a -> c@ and @b -> c@ are mutually exclusive.
record :: MonadState E m => Text -> m ()
record s = case connections s of
  Nothing -> pure ()
  Just xs -> modify f
    where
    f :: E -> E
    f m = Map.unionsWith Set.intersection $ m : ((`Map.singleton` xs) <$> Text.unpack s)

connections :: Text -> Maybe (Set Char)
connections s = Map.lookup (Text.length s) segmentsLengths

segmentsLengths :: Map Int (Set Char)
segmentsLengths = Map.fromListWith Set.union $ fmap (\s -> (length s, Set.fromList s))
  [ "cf"
  , "acf"
  , "bcfd"
  , "abdfg"
  , "acdeg"
  , "acdfg"
  , "abcdfg"
  , "abcefg"
  , "abdefg"
  , "abcdefg"
  ]

-- A configuration is a permutation on Z/7.
