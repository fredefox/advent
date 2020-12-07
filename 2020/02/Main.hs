{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language TypeApplications #-}
{-# OPTIONS_GHC -Wall -Werror #-}
import Text.ParserCombinators.ReadP
import Text.Read (readPrec, readP_to_Prec, readS_to_Prec)
import qualified Text.Read as Read

main :: IO ()
main = do
  print $ read @Entry "1-18 c: ccdczcvccvclcccvkccb"
  input <- getInput
  print $ length $ filter valid input
  print $ length $ filter valid2 input

getInput :: IO [Entry]
getInput = fmap (read @Entry) . lines <$> getContents

valid :: Entry -> Bool
valid (Entry i j c s) = i <= n && n <= j
  where
  n = length $ filter (== c) s

valid2 :: Entry -> Bool
valid2 (Entry i j c s) = length cs == 1
  where
  cs = filter (== c) $ fmap fst $ filter (\(_, n) -> n `elem` [i, j]) $ zip s [1..]

data Entry = Entry Int Int Char String

instance Read Entry where
  readPrec = do
    n <- readPrec @Int
    _ <- readP_to_Prec $ const $ char '-'
    m <- readPrec @Int
    readP_to_Prec $ const skipSpaces
    c <- Read.get
    _ <- readP_to_Prec $ const $ char ':'
    readP_to_Prec $ const skipSpaces
    s <- readS_to_Prec $ const lex
    pure $ Entry n m c s

deriving stock instance Show Entry
