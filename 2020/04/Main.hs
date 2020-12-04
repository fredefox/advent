{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Monoid
import Data.Coerce
import Text.Read

main :: IO ()
main = do
  input <- parse <$> Text.getContents
  -- Text.putStr $ solve hasRequiredProps input
  Text.putStr $ solve valid input

solve :: ([(Text, Text)] -> Bool) -> [[(Text, Text)]] -> Text
solve p = Text.unlines . fmap (Text.pack . show) . filter p

valid :: [(Text, Text)] -> Bool
valid = coerce (mconcat @([(Text, Text)] -> All)) [hasRequiredProps, byr, iyr, eyr, hgt, hcl, ecl, pid, cid]

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.
byr, iyr, eyr, hgt, hcl, ecl, pid, cid :: [(Text, Text)] -> Bool
byr = liftRange "byr" 1920 2002
iyr = liftRange "iyr" 2010 2020
eyr = liftRange "eyr" 2020 2030
hgt = liftProp "hgt" $ \x -> case readHeight @Int $ Text.unpack x of
  Nothing -> False
  Just (n, s) -> (s == "in" && 59 <= n && n <= 76) || (s == "cm" && 150 <= n && n <= 193)
hcl = liftProp "hcl" $ \x -> Text.length x == 7 && Text.all (`elem` ['#'] <> ['0'..'9'] <> ['a'..'f']) x
ecl = liftProp "ecl" $ \x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
pid = liftProp "pid" $ \x -> Text.length x == 9 && Text.all (`elem` ['0'..'9']) x
cid = const True

readHeight :: Read a => String -> Maybe (a, String)
readHeight s = case reads s of
  [(n, s')] -> pure (n, s')
  _        -> Nothing

liftRange :: Text -> Int -> Int -> [(Text, Text)] -> Bool
liftRange nm a b = liftProp nm $ \x -> case readMaybe @Int $ Text.unpack x of
  Nothing -> False
  Just n -> a <= n && n <= b

liftProp :: Text -> (Text -> Bool) -> [(Text, Text)] -> Bool
liftProp nm p m = case lookup nm m of
  Nothing -> False
  Just x -> p x

hasRequiredProps :: [(Text, Text)] -> Bool
hasRequiredProps xs = fmap fst xs `subset` requiredProps
  where
  requiredProps :: [] Text
  requiredProps =
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    ]

subset :: Ord a => [] a -> [] a -> Bool
subset a b = null $ b \\ a

parse :: Text -> [[(Text, Text)]]
parse = fmap (fmap f . Text.words . Text.unwords . Text.lines) . Text.splitOn "\n\n"
  where
  f :: Text -> (Text, Text)
  f s = case Text.splitOn ":" s of
    (x:ys) -> (x, Text.intercalate ":" ys)
    _       -> (s, mempty)
