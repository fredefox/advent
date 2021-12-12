{-# language TypeApplications #-}
import qualified Data.Graph as Graph
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable
import Data.String
import Data.Maybe
import qualified Data.Array as Array
import qualified Data.Map as Map

main :: IO ()
main = do
  let pr (x:y:_) = (x, [y])
  xs <- fmap (pr . Text.splitOn (fromString "-")) . Text.lines <$> Text.getContents
  let xs' = Map.toList $ Map.fromListWith (<>) $ foldMap (\(k, vs) -> (k, vs) : ((\v -> (v, [k])) <$> vs)) xs
  print xs'
  let (g, k, l) = Graph.graphFromEdges @Text $ (\(t, u) -> (t, t, u)) <$> xs'
  -- let (g, k, l) = Graph.buildG (0, length xs) xs
  traverse_ print xs'
  let name v = case k v of (x, _, _) -> x
  traverse_ print $ (fmap name) <$> Graph.dfs g [fromMaybe undefined (l (fromString "start"))]
  let (a, b) = Array.bounds g
  traverse_ print $ k <$> [a..b]
  print $ k <$> l (fromString "start")
