{-# language LambdaCase #-}
import Data.Foldable (traverse_, foldl')
import qualified Data.List
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  xs <- lines <$> getContents
  let ys = stripMatches <$> xs
  print $ sum $ solve . fst <$> ys
  let scores = score . fst <$> filter ((== Incomplete) . snd) ys
  print $ middle $ Data.List.sort scores

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

score :: String -> Int
score = foldl' f 0
  where
  f :: Int -> Char -> Int
  f n c = n * 5 + val c
  val = \case
    '(' -> 1
    '[' -> 2
    '{' -> 3
    '<' -> 4
    _   -> 0

solve :: String -> Int
solve = \case
  [] -> 0
  (x:_) -> value x
  where
  value c = fromMaybe 0 $ lookup c [')' |> 3, ']' |> 57, '}' |> 1197, '>' |> 25137]

data Status = Good | Incomplete | Mismatch deriving (Show, Eq)

type Parser a = String -> (String, a)

stripMatches :: Parser Status
stripMatches = parser mempty
  where
  parser :: String -> Parser Status
  parser s = \case
    [] -> (s, case s of { [] -> Good ; _ -> Incomplete })
    xs@(x:xss) -> case open x of
      Nothing -> parser (x:s) xss
      Just x' -> case s of
        []     -> (xs, Mismatch)
        (y:ys) -> if x' == y then parser ys xss else (xs, Mismatch)

-- | Get corresponding open bracket.
open :: Char -> Maybe Char
open = (`lookup` brackets)

brackets :: [(Char, Char)]
brackets =
  [ ')' |> '('
  , ']' |> '['
  , '}' |> '{'
  , '>' |> '<'
  ]

(|>) :: a -> b -> (a, b)
(|>) = (,)
