{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module       : Data.IntMap.Monoidal.Lazy
-- Copyright    : (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- A version of @Data.IntMap.Lazy@ where the default combining operations
-- require the values to form a 'Semigroup', so that they may be merged
-- instead of overwritten as is the default in @Data.IntMap@.
--
-- This module is intended to duplicate the functionality from @Data.IntMap.Lazy@
-- as closely as possible. Most functions are simply newtype synonyms for the
-- ones found in the original module, and are provided here for similarity and convenience.
module Data.IntMap.Monoidal.Lazy (
    -- * Map type
    IntMap (..),

    -- * Construction
    empty,
    singleton,
    fromSet,

    -- ** From Unordered Lists
    fromList,
    fromListWith,
    fromListWithKey,

    -- ** From Ascending Lists
    fromAscList,
    fromAscListWith,
    fromAscListWithKey,
    fromDistinctAscList,

    -- * Insertion
    insert,
    insertWith,
    insertWithKey,
    insertLookupWithKey,
    overwrite,
    overwriteLookup,

    -- * Deletion\/Update
    delete,
    adjust,
    adjustWithKey,
    update,
    updateWithKey,
    updateLookupWithKey,
    alter,
    alterF,

    -- * Query

    -- ** Lookup
    lookup,
    (!?),
    (!!),
    (!),
    findWithDefault,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,

    -- ** Size
    null,
    size,

    -- * Combine

    -- ** Union
    union,
    unionWith,
    unionWithKey,
    unions,
    unionsWith,

    -- ** Difference
    difference,
    (\\),
    differenceWith,
    differenceWithKey,

    -- ** Intersection
    intersection,
    intersectionWith,
    intersectionWithKey,

    -- * Traversal

    -- ** Map
    map,
    mapWithKey,
    traverseWithKey,
    mapAccum,
    mapAccumR,
    mapAccumWithKey,
    mapAccumRWithKey,
    mapKeys,
    mapKeysWith,
    mapKeysMonotonic,
    traverseKeys,
    traverseKeysWith,

    -- * Folds
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    foldMapWithKey,

    -- ** Strict folds
    foldr',
    foldl',
    foldrWithKey',
    foldlWithKey',
    foldMapWithKey',

    -- * Conversion
    elems,
    keys,
    assocs,
    keysSet,

    -- ** Lists
    toList,

    -- ** Ordered lists
    toAscList,
    toDescList,

    -- * Filter
    filter,
    filterWithKey,
    restrictKeys,
    withoutKeys,
    partition,
    partitionWithKey,
    mapMaybe,
    mapMaybeWithKey,
    mapEither,
    mapEitherWithKey,
    split,
    splitLookup,
    splitRoot,

    -- * Submap
    isSubmapOf,
    isSubmapOfBy,
    isProperSubmapOf,
    isProperSubmapOfBy,

    -- * Min\/Max
    lookupMin,
    lookupMax,
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    deleteFindMin,
    deleteFindMax,
    updateMin,
    updateMax,
    updateMinWithKey,
    updateMaxWithKey,
    minView,
    maxView,
    minViewWithKey,
    maxViewWithKey,
) where

import Data.Bifunctor (bimap)
import Data.Data (Data)
import Data.Foldable.Toolbox (Foldable (fold, foldl', foldr', toList))
import Data.Function.Toolbox ((.:))
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import qualified Data.IntMap.Lazy as Map
import Data.IntSet (IntSet)
import Data.Maybe (fromMaybe)
import Data.Tuple.Toolbox ((***))
import GHC.Exts (IsList)
import Prelude hiding (
    drop,
    filter,
    lookup,
    map,
    null,
    splitAt,
    take,
    (!!),
 )

newtype IntMap a = IntMap {unIntMap :: Map.IntMap a}
    deriving stock (Eq, Ord, Show, Read, Data)
    deriving newtype (Functor, Foldable, IsList, Eq1, Ord1, Show1, Read1)

instance (Semigroup a) => Semigroup (IntMap a) where
    IntMap m <> IntMap n = IntMap $ Map.unionWith (<>) m n
instance (Semigroup a) => Monoid (IntMap a) where
    mempty = IntMap mempty

instance Traversable IntMap where
    traverse f (IntMap m) = IntMap <$> traverse f m

-- | /O(1)/. The empty map.
empty :: IntMap a
empty = IntMap Map.empty

-- | /O(1)/. A map with a single element.
singleton :: Int -> a -> IntMap a
singleton = IntMap .: Map.singleton

-- | /O(n)/. Return all X of the map in ascending order of their keys. Subject to list fusion.
elems :: IntMap a -> [a]
elems (IntMap m) = Map.elems m

-- | /O(n)/. Return all X of the map in ascending order. Subject to list fusion.
keys :: IntMap a -> [Int]
keys (IntMap m) = Map.keys m

-- | /O(n)/. Return all X of the map in ascending key order. Subject to list fusion.
assocs :: IntMap a -> [(Int, a)]
assocs (IntMap m) = Map.assocs m

-- | /O(n)/. The set of all keys of the map.
keysSet :: IntMap a -> IntSet
keysSet (IntMap m) = Map.keysSet m

-- | /O(n)/. Build a map from a set of keys and a function
--   which for each key computes its value.
fromSet :: (Int -> a) -> IntSet -> IntMap a
fromSet = IntMap .: Map.fromSet

-- | /O(n log n)/. Build a map from a list of key/value pairs.
--   See also 'fromAscList'.
--   If the list contains more than one value for the same key,
--   the values are combined using '(<>)'.
fromList :: (Semigroup a) => [(Int, a)] -> IntMap a
fromList = IntMap . Map.fromListWith (<>)

-- | /O(n log n)/. Build a map from a list of key/value pairs with a combining function.
--   See also 'fromAscListWith'.
fromListWith :: (a -> a -> a) -> [(Int, a)] -> IntMap a
fromListWith = IntMap .: Map.fromListWith

-- | /O(n log n)/. Build a map from a list of key/value pairs with a combining function.
--   See also 'fromAscListWithKey'.
fromListWithKey :: (Int -> a -> a -> a) -> [(Int, a)] -> IntMap a
fromListWithKey = IntMap .: Map.fromListWithKey

-- | /O(n)/. Build a map from an ascending list in linear time.
--   /The precondition (input list is ascending) is not checked./
--   If the list contains more than one value for the same key,
--   the values are combined using '(<>)'.
fromAscList :: (Semigroup a) => [(Int, a)] -> IntMap a
fromAscList = IntMap . Map.fromAscListWith (<>)

-- | /O(n)/. Build a map from an ascending list in linear time
--   with a combining function for equal keys.
--   /The precondition (input list is ascending) is not checked./
fromAscListWith :: (a -> a -> a) -> [(Int, a)] -> IntMap a
fromAscListWith = IntMap .: Map.fromAscListWith

-- | /O(n)/. Build a map from an ascending list in linear time
--   with a combining function for equal keys.
--   /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: (Int -> a -> a -> a) -> [(Int, a)] -> IntMap a
fromAscListWithKey = IntMap .: Map.fromAscListWithKey

-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
--   /The precondition is not checked./
fromDistinctAscList :: [(Int, a)] -> IntMap a
fromDistinctAscList = IntMap . Map.fromDistinctAscList

-- | /O(log n)/. Insert a new key and value in the map. If the key is already
--   present in the map, the associated value is combined with the new value using '(<>)'.
--   To overwrite the existing value instead, use 'overwrite'.
--
--   __NOTE:__ For noncommutative semigroups (e.g. lists), the value at key @k@ in @'insert' k new m@
--   will be @new <> old@. Use @'insertWith' (flip (<>)) k new@ if you want @old <> new@.
insert :: (Semigroup a) => Int -> a -> IntMap a -> IntMap a
insert k a (IntMap m) = IntMap $ Map.insertWith (<>) k a m

-- | /O(log n)/. Insert a new key and value in the map. If the key is already
--   present in the map, the associated value is replaced by the new value.
--
-- > 'overwrite' == 'insertWith' 'const'
overwrite :: Int -> a -> IntMap a -> IntMap a
overwrite k a (IntMap m) = IntMap $ Map.insert k a m

-- | /O(log n)/. Combines replacement and retrieval.
--
-- > 'overwriteLookup' k a == 'lookup' k &&& 'overwrite' k a
overwriteLookup :: Int -> a -> IntMap a -> (Maybe a, IntMap a)
overwriteLookup k a (IntMap m) = IntMap <$> Map.insertLookupWithKey (const const) k a m

-- | /O(log n)/. Insert with a function, combining new value and old value
--   using the supplied function.
--
--   @'insertWith' f k new m@ will insert @new@ at @k@ if there is no value present,
--   or @f new old@ if there was already a value @old@ at @k@.
insertWith :: (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWith f k a (IntMap m) = IntMap $ Map.insertWith f k a m

-- | /O(log n)/. Insert with a function, combining new value and old value
--   using the supplied function.
--
--   @'insertWith' f k new m@ will insert @new@ at @k@ if there is no value present,
--   or @f k new old@ if there was already a value @old@ at @k@.
--   The key passed to @f@ is the one passed to 'insertWithKey', not the one present in the map.
insertWithKey :: (Int -> a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWithKey f k a (IntMap m) = IntMap $ Map.insertWithKey f k a m

-- | /O(log n)/. Combines insertion and retrieval.
--
-- > 'insertLookupWithKey' f k new == 'lookup' k &&& 'insertWithKey' f k new
insertLookupWithKey :: (Int -> a -> a -> a) -> Int -> a -> IntMap a -> (Maybe a, IntMap a)
insertLookupWithKey f k a (IntMap m) = IntMap <$> Map.insertLookupWithKey f k a m

-- | /O(log n)/. Delete a key and its value from the map, or do nothing if the key is missing.
delete :: Int -> IntMap a -> IntMap a
delete k (IntMap m) = IntMap $ Map.delete k m

-- | /O(log n)/. Change a value at a specific key with the result of the provided function,
--   or do nothing if the key is missing.
adjust :: (a -> a) -> Int -> IntMap a -> IntMap a
adjust f k (IntMap m) = IntMap $ Map.adjust f k m

-- | /O(log n)/. Change a value at a specific key with access to the key itself,
--   or do nothing if the key is missing.
adjustWithKey :: (Int -> a -> a) -> Int -> IntMap a -> IntMap a
adjustWithKey f k (IntMap m) = IntMap $ Map.adjustWithKey f k m

-- | /O(log n)/. Change a value at a specific key. If the function evaluates to @Nothing@,
--   the key and value are removed from the map. If the key is missing, do nothing.
update :: (a -> Maybe a) -> Int -> IntMap a -> IntMap a
update f k (IntMap m) = IntMap $ Map.update f k m

-- | /O(log n)/. Change a value at a specific key with access to the key itself.
--   If the function evaluates to @Nothing@, the key and value are removed from the map.
--   If the key is missing, do nothing.
updateWithKey :: (Int -> a -> Maybe a) -> Int -> IntMap a -> IntMap a
updateWithKey f k (IntMap m) = IntMap $ Map.updateWithKey f k m

-- | /O(log n)/. Combines change and retrieval.
--
-- > 'updateLookupWithKey' f k == 'lookup' k &&& 'updateWithKey' f k
updateLookupWithKey :: (Int -> a -> Maybe a) -> Int -> IntMap a -> (Maybe a, IntMap a)
updateLookupWithKey f k (IntMap m) = IntMap <$> Map.updateLookupWithKey f k m

-- | /O(log n)/. Can be used to 'insert', 'overwrite', 'delete', or 'update' a value.
--
-- > 'alter' (Just new <>) k == 'insert' k new
-- > 'alter' (const (Just new)) k == 'overwrite' k new
-- > 'alter' (const Nothing) k == 'delete' k
-- > 'alter' (fmap f) k == 'adjust' f k
alter :: (Maybe a -> Maybe a) -> Int -> IntMap a -> IntMap a
alter f k (IntMap m) = IntMap $ Map.alter f k m

alterF :: (Functor f) => (Maybe a -> f (Maybe a)) -> Int -> IntMap a -> f (IntMap a)
alterF f k (IntMap m) = IntMap <$> Map.alterF f k m

-- | /O(log n)/. Lookup the value at a key.
lookup :: Int -> IntMap a -> Maybe a
lookup k (IntMap m) = Map.lookup k m

-- | /O(log n)/. A flipped, infix variant of 'lookup'.
(!?) :: IntMap a -> Int -> Maybe a
(!?) = flip lookup

-- | /O(log n)/. A version of '(!?)' that calls 'error' when the element cannot be found.
(!!) :: IntMap a -> Int -> a
(!!) = fromMaybe (error "element not in the map") .: (!?)

-- | /O(log n)/. A version of '(!?)' that returns 'mempty' when the element cannot be found.
--
-- > m ! k == findWithDefault mempty k m
(!) :: (Monoid a) => IntMap a -> Int -> a
(!) = fromMaybe mempty .: (!?)

-- | /O(log n)/. Lookup the value at a key, with a default if the key is missing.
findWithDefault :: a -> Int -> IntMap a -> a
findWithDefault a = fromMaybe a .: lookup

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: Int -> IntMap a -> Bool
member k (IntMap m) = Map.member k m

-- | /O(log n)/. Is the key missing from the map? See also 'member'.
notMember :: Int -> IntMap a -> Bool
notMember k (IntMap m) = Map.notMember k m

-- | Find the next smallest key to the given one, and return its key/value pair.
lookupLT :: Int -> IntMap a -> Maybe (Int, a)
lookupLT k (IntMap m) = Map.lookupLT k m

-- | Find the next largest key to the given one, and return its key/value pair.
lookupGT :: Int -> IntMap a -> Maybe (Int, a)
lookupGT k (IntMap m) = Map.lookupGT k m

-- | Find the largest key up to the given one, and return its key/value pair.
lookupLE :: Int -> IntMap a -> Maybe (Int, a)
lookupLE k (IntMap m) = Map.lookupLE k m

-- | Find the smallest key down to the given one, and return its key/value pair.
lookupGE :: Int -> IntMap a -> Maybe (Int, a)
lookupGE k (IntMap m) = Map.lookupGE k m

-- | /O(1)/. Is the map empty?
null :: IntMap a -> Bool
null (IntMap m) = Map.null m

-- | /O(1)/. The number of elements in the map.
size :: IntMap a -> Int
size (IntMap m) = Map.size m

-- | /O(m log(n \/ m + 1)), m <= n/. Join two maps together using '(<>)' to combine
--   the values of duplicate keys.
--
--   To retain 'Data.Map'\'s left-biased functionality, use @'unionWith' 'const'@.
union :: (Semigroup a) => IntMap a -> IntMap a -> IntMap a
union = (<>)

-- | /O(m log(n \/ m + 1)), m <= n/. Join two maps with a combining function.
unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f (IntMap m) (IntMap n) = IntMap $ Map.unionWith f m n

-- | /O(m log(n \/ m + 1)), m <= n/. Join two maps with a combining function.
unionWithKey :: (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f (IntMap m) (IntMap n) = IntMap $ Map.unionWithKey f m n

-- | A synonym for 'fold'. To retain 'Data.Map'\'s functionality, use @'unionsWith' 'const'@.
unions :: (Foldable t, Semigroup a) => t (IntMap a) -> IntMap a
unions = fold

-- | The union of a list of maps, combining with a specified operation.
unionsWith :: (Foldable t) => (a -> a -> a) -> t (IntMap a) -> IntMap a
unionsWith f = foldl (unionWith f) empty

-- | /O(m log(n \/ m + 1)), m <= n/. The set-difference of the keys in a map,
--   keeping the values of the left-hand map.
difference :: IntMap a -> IntMap b -> IntMap a
difference (IntMap m) (IntMap n) = IntMap $ Map.difference m n

-- | Infix synonym for 'difference'.
(\\) :: IntMap a -> IntMap b -> IntMap a
(\\) = difference

-- | /O(n + m)/. Difference with a combining function. Deletes keys if the value is @Nothing@.
differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f (IntMap m) (IntMap n) = IntMap $ Map.differenceWith f m n

-- | /O(n + m)/. Difference with a combining function. Deletes keys if the value is @Nothing@.
differenceWithKey :: (Int -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithKey f (IntMap m) (IntMap n) = IntMap $ Map.differenceWithKey f m n

-- | /O(m log(n \/ m + 1)), m <= n/. The set-intersection of the keys in a map,
--   keeping the values of the left-hand map.
intersection :: IntMap a -> IntMap b -> IntMap a
intersection (IntMap m) (IntMap n) = IntMap $ Map.intersection m n

-- | /O(m log(n \/ m + 1)), m <= n/. Intersection with a combining function.
intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f (IntMap m) (IntMap n) = IntMap $ Map.intersectionWith f m n

-- | /O(m log(n \/ m + 1)), m <= n/. Intersection with a combining function.
intersectionWithKey :: (Int -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f (IntMap m) (IntMap n) = IntMap $ Map.intersectionWithKey f m n

-- | /O(n)/. IntMap a function over all values in the map.
map :: (a -> b) -> IntMap a -> IntMap b
map = fmap

-- | /O(n)/. Like 'map' but the function has access to the keys.
mapWithKey :: (Int -> a -> b) -> IntMap a -> IntMap b
mapWithKey f (IntMap m) = IntMap $ Map.mapWithKey f m

-- | /O(n)/. Like 'traverse' but the function has access to the keys.
traverseWithKey :: (Applicative f) => (Int -> a -> f b) -> IntMap a -> f (IntMap b)
traverseWithKey f (IntMap m) = IntMap <$> Map.traverseWithKey f m

-- | /O(n)/. Thread an accumulating argument through the map in ascending order of keys.
mapAccum :: (acc -> a -> (acc, b)) -> acc -> IntMap a -> (acc, IntMap b)
mapAccum f a (IntMap m) = IntMap <$> Map.mapAccum f a m

-- | /O(n)/. Thread an accumulating argument through the map in descending order of keys.
mapAccumR :: (acc -> a -> (acc, b)) -> acc -> IntMap a -> (acc, IntMap b)
mapAccumR f a (IntMap m) = IntMap <$> Map.mapAccumRWithKey (const . f) a m

-- | /O(n)/. Like 'mapAccum' but the function has access to the keys.
mapAccumWithKey :: (acc -> Int -> a -> (acc, b)) -> acc -> IntMap a -> (acc, IntMap b)
mapAccumWithKey f acc (IntMap m) = IntMap <$> Map.mapAccumWithKey f acc m

-- | /O(n)/. Like 'mapAccumR' but the function has access to the keys.
mapAccumRWithKey :: (acc -> Int -> a -> (acc, b)) -> acc -> IntMap a -> (acc, IntMap b)
mapAccumRWithKey f acc (IntMap m) = IntMap <$> Map.mapAccumRWithKey f acc m

-- | /O(n log n)/. IntMap a function over the keys of the map.
--   If the function maps two or more @j@-keys to the same @k@-key,
--   the values of the @j@-keys are combined with '(<>)'.
--
--   To retain 'Data.Map'\'s greatest-biased functionality, use @'mapKeysWith' 'const'@.
mapKeys :: (Semigroup a) => (Int -> Int) -> IntMap a -> IntMap a
mapKeys f (IntMap m) = IntMap $ Map.mapKeysWith (<>) f m

-- | /O(n log n)/. IntMap a function over the keys of the map with a value-combining function.
mapKeysWith :: (a -> a -> a) -> (Int -> Int) -> IntMap a -> IntMap a
mapKeysWith c f (IntMap m) = IntMap $ Map.mapKeysWith c f m

-- | /O(n)/. IntMap a strictly-monotonic function over the keys of the map.
mapKeysMonotonic :: (Int -> Int) -> IntMap a -> IntMap a
mapKeysMonotonic f (IntMap m) = IntMap $ Map.mapKeysMonotonic f m

-- | /O(n log n)/. IntMap an applicative function over the keys of the map and collect the results.
traverseKeys :: (Applicative f, Semigroup a) => (Int -> f Int) -> IntMap a -> f (IntMap a)
traverseKeys f = fmap fromList . traverse (\(j, a) -> (,a) <$> f j) . assocs

-- | /O(n log n)/. IntMap an applicative function over the keys of the map
--   and collect the results using the specified combining function.
traverseKeysWith :: (Applicative f) => (a -> a -> a) -> (Int -> f Int) -> IntMap a -> f (IntMap a)
traverseKeysWith c f = fmap (fromListWith c) . traverse (\(j, a) -> (,a) <$> f j) . assocs

-- | /O(n)/. Fold the keys and values in the map using the given right-associative binary operator.
foldrWithKey :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z (IntMap m) = Map.foldrWithKey f z m

-- | /O(n)/. Fold the keys and values in the map using the given left-associative binary operator.
foldlWithKey :: (a -> Int -> b -> a) -> a -> IntMap b -> a
foldlWithKey f z (IntMap m) = Map.foldlWithKey f z m

-- | /O(n)/. Fold the keys and values in the map strictly,
--   using the given right-associative binary operator.
foldrWithKey' :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey' f z (IntMap m) = Map.foldrWithKey' f z m

-- | /O(n)/. Fold the keys and values in the map strictly,
--   using the given left-associative binary operator.
foldlWithKey' :: (a -> Int -> b -> a) -> a -> IntMap b -> a
foldlWithKey' f z (IntMap m) = Map.foldlWithKey' f z m

-- | /O(n)/. Fold the keys and values in the map using the given monoid.
foldMapWithKey :: Monoid m => (Int -> a -> m) -> IntMap a -> m
foldMapWithKey f (IntMap m) = Map.foldMapWithKey f m

-- | /O(n)/. Fold the keys and values in the map strictly using the given monoid.
foldMapWithKey' :: Monoid m => (Int -> a -> m) -> IntMap a -> m
foldMapWithKey' f (IntMap m) = Map.foldMapWithKey (f $!) m

-- | /O(n)/. Convert the map to a list of key/value pairs where the keys are in ascending order.
--   Subject to list fusion.
toAscList :: IntMap a -> [(Int, a)]
toAscList (IntMap m) = Map.toAscList m

-- | /O(n)/. Convert the map to a list of key/value pairs where the keys are in descending order.
--   Subject to list fusion.
toDescList :: IntMap a -> [(Int, a)]
toDescList (IntMap m) = Map.toDescList m

-- | /O(n)/. Filter all values that satisfy the predicate.
filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p (IntMap m) = IntMap $ Map.filter p m

-- | /O(n)/. Filter all key/value pairs that satisfy the predicate.
filterWithKey :: (Int -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey p (IntMap m) = IntMap $ Map.filterWithKey p m

-- | /O(m log(n \/ m + 1)), m <= n/. Restrict a 'Map' to only the keys in a given 'Set'.
restrictKeys :: IntMap a -> IntSet -> IntMap a
restrictKeys (IntMap m) s = IntMap $ Map.restrictKeys m s

-- | /O(m log(n \/ m + 1)), m <= n/. Remove all the keys in a 'Set' from a 'Map'.
withoutKeys :: IntMap a -> IntSet -> IntMap a
withoutKeys (IntMap m) s = IntMap $ Map.withoutKeys m s

-- | /O(n)/. Partition the map according to a predicate (satisfied, failed).
partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition p (IntMap m) = (IntMap *** IntMap) $ Map.partition p m

-- | /O(n)/. Partition the map according to a predicate (satisfied, failed).
partitionWithKey :: (Int -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey p (IntMap m) = (IntMap *** IntMap) $ Map.partitionWithKey p m

-- | /O(n)/. IntMap values and collect the 'Just' results.
mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f (IntMap m) = IntMap $ Map.mapMaybe f m

-- | /O(n)/. IntMap values and collect the 'Just' results.
mapMaybeWithKey :: (Int -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f (IntMap m) = IntMap $ Map.mapMaybeWithKey f m

-- | /O(n)/. IntMap values and collect the 'Just' results.
mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f (IntMap m) = bimap IntMap IntMap $ Map.mapEither f m

-- | /O(n)/. IntMap values and collect the 'Just' results.
mapEitherWithKey :: (Int -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f (IntMap m) = bimap IntMap IntMap $ Map.mapEitherWithKey f m

-- | /O(log n)/. Partition the map by comparing keys ((smaller, larger) than given).
split :: Int -> IntMap a -> (IntMap a, IntMap a)
split k (IntMap m) = bimap IntMap IntMap $ Map.split k m

-- | /O(log n)/. Like 'split' but the middle coordinate 'lookup's the value at the key.
splitLookup :: Int -> IntMap a -> (IntMap a, Maybe a, IntMap a)
splitLookup k (IntMap m) = (\(n, y, p) -> (IntMap n, y, IntMap p)) $ Map.splitLookup k m

-- | /O(1)/. Decompose a map into pieces based on the structure of the underlying tree.
splitRoot :: IntMap a -> [IntMap a]
splitRoot (IntMap m) = IntMap <$> Map.splitRoot m

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ their values all agree.
isSubmapOf :: (Eq a) => IntMap a -> IntMap a -> Bool
isSubmapOf (IntMap m) (IntMap n) = Map.isSubmapOf m n

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ the function returns 'True' when applied to respective values.
isSubmapOfBy :: (a -> a -> Bool) -> IntMap a -> IntMap a -> Bool
isSubmapOfBy f (IntMap m) (IntMap n) = Map.isSubmapOfBy f m n

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ their values all agree, __and__ the maps are not equal.
isProperSubmapOf :: (Eq a) => IntMap a -> IntMap a -> Bool
isProperSubmapOf (IntMap m) (IntMap n) = Map.isProperSubmapOf m n

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ the function returns 'True' when applied to respective values,
--   __and__ the maps are not equal.
isProperSubmapOfBy :: (a -> a -> Bool) -> IntMap a -> IntMap a -> Bool
isProperSubmapOfBy f (IntMap m) (IntMap n) = Map.isProperSubmapOfBy f m n

-- | /O(log n)/. The minimal key of the map, or 'Nothing' if the map is empty.
lookupMin :: IntMap a -> Maybe (Int, a)
lookupMin (IntMap m) = Map.lookupMin m

-- | /O(log n)/. The maximal key of the map, or 'Nothing' if the map is empty.
lookupMax :: IntMap a -> Maybe (Int, a)
lookupMax (IntMap m) = Map.lookupMax m

-- | /O(log n)/. The minimal key of the map, or 'error' if the map is empty.
findMin :: IntMap a -> (Int, a)
findMin (IntMap m) = Map.findMin m

-- | /O(log n)/. The maximal key of the map, or 'error' if the map is empty.
findMax :: IntMap a -> (Int, a)
findMax (IntMap m) = Map.findMax m

-- | /O(log n)/. Delete the minimal key.
deleteMin :: IntMap a -> IntMap a
deleteMin (IntMap m) = IntMap $ Map.deleteMin m

-- | /O(log n)/. Delete the maximal key.
deleteMax :: IntMap a -> IntMap a
deleteMax (IntMap m) = IntMap $ Map.deleteMax m

-- | /O(log n)/. Delete and return the minimal key of the map, or 'error' if the map is empty.
deleteFindMin :: IntMap a -> ((Int, a), IntMap a)
deleteFindMin (IntMap m) = IntMap <$> Map.deleteFindMin m

-- | /O(log n)/. Delete and return the maximal key of the map, or 'error' if the map is empty.
deleteFindMax :: IntMap a -> ((Int, a), IntMap a)
deleteFindMax (IntMap m) = IntMap <$> Map.deleteFindMax m

-- | /O(log n)/. Update the value at the minimal key.
updateMin :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMin f (IntMap m) = IntMap $ Map.updateMin f m

-- | /O(log n)/. Update the value at the maximal key.
updateMax :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMax f (IntMap m) = IntMap $ Map.updateMax f m

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (Int -> a -> Maybe a) -> IntMap a -> IntMap a
updateMinWithKey f (IntMap m) = IntMap $ Map.updateMinWithKey f m

-- | /O(log n)/. Update the value at the maximal key.
updateMaxWithKey :: (Int -> a -> Maybe a) -> IntMap a -> IntMap a
updateMaxWithKey f (IntMap m) = IntMap $ Map.updateMaxWithKey f m

-- | /O(log n)/. Retrieve the value associated with the minimal key of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
minView :: IntMap a -> Maybe (a, IntMap a)
minView (IntMap m) = fmap IntMap <$> Map.minView m

-- | /O(log n)/. Retrieve the value associated with the maximal key of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
maxView :: IntMap a -> Maybe (a, IntMap a)
maxView (IntMap m) = fmap IntMap <$> Map.maxView m

-- | /O(log n)/. Retrieve the minimal key/value pair of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
minViewWithKey :: IntMap a -> Maybe ((Int, a), IntMap a)
minViewWithKey (IntMap m) = fmap IntMap <$> Map.minViewWithKey m

-- | /O(log n)/. Retrieve the maximal key/value pair of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
maxViewWithKey :: IntMap a -> Maybe ((Int, a), IntMap a)
maxViewWithKey (IntMap m) = fmap IntMap <$> Map.maxViewWithKey m
