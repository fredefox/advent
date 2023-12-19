{-# language DerivingStrategies, LambdaCase #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Applicative
import qualified Data.List as List
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
  (m, objs) <- parse <$> getContents
  let l = "in"
  print $ sum $ value <$> filter (\obj -> run m obj l) objs

value :: Obj -> Int
value (Obj o) = sum $ Map.elems o

run :: Map String [Rule] -> Obj -> String -> Bool
run m obj l = s == "A"
  where
  s = head $ dropWhile (`notElem` ["A", "R"]) $ iterate (step m obj) l

parse :: String -> (Map String [Rule], [Obj])
parse s = case Text.splitOn (Text.pack "\n\n") (Text.pack s) of
  (a:b:_) -> (Map.fromList $ readWorkflow <$> lines (Text.unpack a), fmap (read @Obj) . lines $ Text.unpack b)
  _ -> error "Parse error"

step :: Map String [Rule] -> Obj -> String -> String
step m o l = case Map.lookup l m of
  Just rs -> stepRule rs
  Nothing -> error "Invalid label"
  where
  stepRule :: [Rule] -> String
  stepRule = \case
    [] -> error "No more steps"
    (x:xs) -> fromMaybe (stepRule xs) $ o `stepObj` x

stepObj :: Obj -> Rule -> Maybe String
stepObj o = \case
  Cond a b -> if o `matches` a then Just b else Nothing
  Uncond l -> Just l

matches :: Obj -> Op -> Bool
matches (Obj o) = \case
  OpLT prop b -> case Map.lookup prop o of
    Nothing -> error "prop not found"
    Just v -> v < b
  OpGT prop b -> case Map.lookup prop o of
    Nothing -> error "prop not found"
    Just v -> v > b

data Obj = Obj (Map String Int)

deriving stock instance Show Obj
instance Read Obj where
  readsPrec _ s = pure (Obj $ Map.fromList o, mempty)
    where
    o = fmap readEq $ split (== ',') $ filter (not . (`elem` ['{', '}'])) s

readEq s = case split (=='=') s of
  (a:b:_) -> (a, read @Int b)
  _ -> error "Parse error"

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = fmap go
  where
  go x = if x == a then b else x

readWorkflow :: String -> (String, [Rule])
readWorkflow s = (a, fmap (read @Rule) $ split (`elem` (","::String)) $ filter (not . (`elem` ['{', '}'])) b)
  where
  (a, b) = span (/= '{') s

data Op = OpLT String Int | OpGT String Int

deriving stock instance Show Op

instance Read Op where
  readsPrec _ s = lt <|> gt
    where
    lt, gt :: [(Op, String)]
    lt = case split (=='<') s of
      (a:b:_) -> pure $ (OpLT a $ read @Int b, mempty)
      _ -> empty
    gt = case split (=='>') s of
      (a:b:_) -> pure $ (OpGT a $ read @Int b, mempty)
      _ -> empty

split _ [] = []
split p s = case span (not . p) s of
  (a, b) -> a : split p (dropWhile p b)

data Rule = Cond Op String | Uncond String

deriving stock instance Show Rule

instance Read Rule where
  readsPrec _ s = pure (r, mempty)
    where
    r = case split (==':') s of
      (a:b:_) -> Cond (read @Op a) b
      _ -> Uncond s
