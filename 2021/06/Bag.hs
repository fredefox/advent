{-# language TypeFamilies #-}
{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TupleSections #-}
{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
module Bag (Bag, map, singleton, size, filter, bag, fromList, toList) where

import Prelude hiding (filter, map)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Exts (IsList)
import qualified GHC.Exts

newtype Bag n a = Bag (Map a n)

deriving stock instance (Show n, Show a) => Show (Bag n a)

instance (Num n, Ord a) => Semigroup (Bag n a) where
  Bag a <> Bag b = Bag $ Map.unionWith (+) a b

instance (Num n, Ord a) => Monoid (Bag n a) where
  mempty = Bag mempty

instance (Integral n, Ord a) => IsList (Bag n a) where
  type Item (Bag n a) = (a, n)
  fromList :: [(a, n)] -> Bag n a
  fromList = Bag . Map.fromListWith (+)
  toList :: Bag n a -> [(a, n)]
  toList (Bag m) = GHC.Exts.toList m

fromList :: forall a n . Integral n => Ord a => [(a, n)] -> Bag n a
fromList = GHC.Exts.fromList @(Bag n a)
{-# INLINE fromList #-}

toList :: forall a n . Integral n => Ord a => Bag n a -> [(a, n)]
toList = GHC.Exts.toList @(Bag n a)
{-# INLINE toList #-}

  -- |
-- >>> fold (fromList [("a", 3), ("b", 1), ("b", 1)])
-- "aaabb"
instance Integral n => Foldable (Bag n) where
  foldMap :: Monoid m => (a -> m) -> Bag n a -> m
  foldMap f (Bag m) = Map.foldMapWithKey (\a n -> mconcat $ replicate (toInt n) $ f a) m

toInt :: forall n . Integral n => n -> Int
toInt = fromInteger @Int . toInteger @n

map :: Num n => Ord a => Ord b => (a -> b) -> Bag n a -> Bag n b
map f (Bag m) = Bag $ Map.mapKeysWith (+) f m

singleton :: a -> n -> Bag n a
singleton a = Bag . Map.singleton a

size :: Num n => Bag n a -> n
size (Bag m) = sum m

filter :: (a -> Bool) -> Bag n a -> Bag n a
filter p (Bag m) = Bag $ Map.filterWithKey (\a _ -> p a) m

bag :: Ord a => Integral n => [a] -> Bag n a
bag = fromList . fmap (,1)
