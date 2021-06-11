{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module       : Data.Map.Monoidal.Lazy
-- Copyright    : (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- A version of 'Data.Map.Lazy' where the default combining operations
-- require the values to form a 'Semigroup', so that they may be merged
-- instead of overwritten as is the default in 'Data.Map'.
--
-- This module is intended to duplicate the functionality from 'Data.Map.Lazy'
-- as closely as possible. Most functions are simply newtype synonyms for the
-- ones found in the original module, and are provided here for similarity and convenience.
module Data.Map.Monoidal.Lazy (
    -- * Map type
    Map (..),

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

    -- ** From Descending Lists
    fromDescList,
    fromDescListWith,
    fromDescListWithKey,
    fromDistinctDescList,

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
    traverseMaybeWithKey,
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
    invertKeys,

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
    takeWhileAntitone,
    dropWhileAntitone,
    spanAntitone,
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

    -- * Indexed
    lookupIndex,
    findIndex,
    elemAt,
    updateAt,
    deleteAt,
    take,
    drop,
    splitAt,

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
import Data.Functor.Classes (Eq1, Eq2, Ord1, Ord2, Read1, Show1, Show2)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
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

newtype Map k a = Map {unMap :: Map.Map k a}
    deriving stock (Eq, Ord, Show, Read, Data)
    deriving newtype (Functor, Foldable, IsList, Eq1, Ord1, Show1, Read1, Eq2, Ord2, Show2)

instance (Ord k, Semigroup a) => Semigroup (Map k a) where
    Map m <> Map n = Map $ Map.unionWith (<>) m n
instance (Ord k, Semigroup a) => Monoid (Map k a) where
    mempty = Map mempty

instance Traversable (Map k) where
    traverse f (Map m) = Map <$> traverse f m

-- | /O(1)/. The empty map.
empty :: Map k a
empty = Map Map.empty

-- | /O(1)/. A map with a single element.
singleton :: k -> a -> Map k a
singleton = Map .: Map.singleton

-- | /O(n)/. Return all X of the map in ascending order of their keys. Subject to list fusion.
elems :: Map k a -> [a]
elems (Map m) = Map.elems m

-- | /O(n)/. Return all X of the map in ascending order. Subject to list fusion.
keys :: Map k a -> [k]
keys (Map m) = Map.keys m

-- | /O(n)/. Return all X of the map in ascending key order. Subject to list fusion.
assocs :: Map k a -> [(k, a)]
assocs (Map m) = Map.assocs m

-- | /O(n)/. The set of all keys of the map.
keysSet :: Map k a -> Set k
keysSet (Map m) = Map.keysSet m

-- | /O(n)/. Build a map from a set of keys and a function
--   which for each key computes its value.
fromSet :: (k -> a) -> Set k -> Map k a
fromSet = Map .: Map.fromSet

-- | /O(n log n)/. Build a map from a list of key/value pairs.
--   See also 'fromAscList'.
--   If the list contains more than one value for the same key,
--   the values are combined using '(<>)'.
fromList :: (Ord k, Semigroup a) => [(k, a)] -> Map k a
fromList = Map . Map.fromListWith (<>)

-- | /O(n log n)/. Build a map from a list of key/value pairs with a combining function.
--   See also 'fromAscListWith'.
fromListWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith = Map .: Map.fromListWith

-- | /O(n log n)/. Build a map from a list of key/value pairs with a combining function.
--   See also 'fromAscListWithKey'.
fromListWithKey :: (Ord k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey = Map .: Map.fromListWithKey

-- | /O(n)/. Build a map from an ascending list in linear time.
--   /The precondition (input list is ascending) is not checked./
--   If the list contains more than one value for the same key,
--   the values are combined using '(<>)'.
fromAscList :: (Eq k, Semigroup a) => [(k, a)] -> Map k a
fromAscList = Map . Map.fromAscListWith (<>)

-- | /O(n)/. Build a map from an ascending list in linear time
--   with a combining function for equal keys.
--   /The precondition (input list is ascending) is not checked./
fromAscListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWith = Map .: Map.fromAscListWith

-- | /O(n)/. Build a map from an ascending list in linear time
--   with a combining function for equal keys.
--   /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWithKey = Map .: Map.fromAscListWithKey

-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
--   /The precondition is not checked./
fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList = Map . Map.fromDistinctAscList

-- | /O(n)/. Build a map from a descending list in linear time.
--   /The precondition (input list is descending) is not checked./
--   If the list contains more than one value for the same key,
--   the values are combined using '(<>)'.
fromDescList :: (Eq k, Semigroup a) => [(k, a)] -> Map k a
fromDescList = Map . Map.fromDescListWith (<>)

-- | /O(n)/. Build a map from a descending list in linear time
--   with a combining function for equal keys.
--   /The precondition (input list is descending) is not checked./
fromDescListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWith = Map .: Map.fromDescListWith

-- | /O(n)/. Build a map from a descending list in linear time
--   with a combining function for equal keys.
--   /The precondition (input list is descending) is not checked./
fromDescListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWithKey = Map .: Map.fromDescListWithKey

-- | /O(n)/. Build a map from a descending list of distinct elements in linear time.
--   /The precondition is not checked./
fromDistinctDescList :: [(k, a)] -> Map k a
fromDistinctDescList = Map . Map.fromDistinctDescList

-- | /O(log n)/. Insert a new key and value in the map. If the key is already
--   present in the map, the associated value is combined with the new value using '(<>)'.
--   The value at key @k@ in @'insert' k new m@ will be @old <> new@,
--   so the overwriting behaviour from @containers@ can be recovered
--   by wrapping values in 'Data.Semigroup.Last' or by using 'overwrite'.
insert :: (Ord k, Semigroup a) => k -> a -> Map k a -> Map k a
insert k a (Map m) = Map $ Map.insertWith (flip (<>)) k a m

-- | /O(log n)/. Insert a new key and value in the map. If the key is already
--   present in the map, the associated value is replaced by the new value.
--
-- > overwrite == insertWith const
overwrite :: (Ord k) => k -> a -> Map k a -> Map k a
overwrite k a (Map m) = Map $ Map.insert k a m

-- | /O(log n)/. Combines replacement and retrieval.
--
-- > overwriteLookup k a == lookup k &&& overwrite k a
overwriteLookup :: (Ord k) => k -> a -> Map k a -> (Maybe a, Map k a)
overwriteLookup k a (Map m) = Map <$> Map.insertLookupWithKey (const const) k a m

-- | /O(log n)/. Insert with a function, combining new value and old value
--   using the supplied function.
--
--   @'insertWith' f k new m@ will insert @new@ at @k@ if there is no value present,
--   or overwrite with @old `f` new@ if there was already a value @old@ at @k@.
insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k a (Map m) = Map $ Map.insertWith (flip f) k a m

-- | /O(log n)/. Insert with a function, combining new value and old value
--   using the supplied function.
--
--   @'insertWith' f k new m@ will insert @new@ at @k@ if there is no value present,
--   or overwrite with @f k old new@ if there was already a value @old@ at @k@.
--   The key passed to @f@ is the one passed to 'insertWithKey', not the one present in the map.
insertWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k a (Map m) = Map $ Map.insertWithKey (flip . f) k a m

-- | /O(log n)/. Combines insertion and retrieval.
--
-- > 'insertLookupWithKey' f k new == 'lookup' k &&& 'insertWithKey' f k new
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f k a (Map m) = Map <$> Map.insertLookupWithKey f k a m

-- | /O(log n)/. Delete a key and its value from the map, or do nothing if the key is missing.
delete :: (Ord k) => k -> Map k a -> Map k a
delete k (Map m) = Map $ Map.delete k m

-- | /O(log n)/. Change a value at a specific key with the result of the provided function,
--   or do nothing if the key is missing.
adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjust f k (Map m) = Map $ Map.adjust f k m

-- | /O(log n)/. Change a value at a specific key with access to the key itself,
--   or do nothing if the key is missing.
adjustWithKey :: (Ord k) => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k (Map m) = Map $ Map.adjustWithKey f k m

-- | /O(log n)/. Change a value at a specific key. If the function evaluates to 'Nothing',
--   the key and value are removed from the map. If the key is missing, do nothing.
update :: (Ord k) => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k (Map m) = Map $ Map.update f k m

-- | /O(log n)/. Change a value at a specific key with access to the key itself.
--   If the function evaluates to 'Nothing', the key and value are removed from the map.
--   If the key is missing, do nothing.
updateWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k (Map m) = Map $ Map.updateWithKey f k m

-- | /O(log n)/. Combines change and retrieval.
--
-- > 'updateLookupWithKey' f k == 'lookup' k &&& 'updateWithKey' f k
updateLookupWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey f k (Map m) = Map <$> Map.updateLookupWithKey f k m

-- | /O(log n)/. Can be used to 'insert', 'overwrite', 'delete', or 'update' a value.
--
-- > 'alter' (Just new <>) k == 'insert' k new
-- > 'alter' (const (Just new)) k == 'overwrite' k new
-- > 'alter' (const Nothing) k == 'delete' k
-- > 'alter' (fmap f) k == 'adjust' f k
alter :: (Ord k) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k (Map m) = Map $ Map.alter f k m

alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF f k (Map m) = Map <$> Map.alterF f k m

-- | /O(log n)/. Lookup the value at a key.
lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup k (Map m) = Map.lookup k m

-- | /O(log n)/. A flipped, infix variant of 'lookup'.
(!?) :: (Ord k) => Map k a -> k -> Maybe a
(!?) = flip lookup

-- | /O(log n)/. A version of '(!?)' that calls 'error' when the element cannot be found.
(!!) :: (Ord k) => Map k a -> k -> a
(!!) = fromMaybe (error "element not in the map") .: (!?)

-- | /O(log n)/. A version of '(!?)' that returns 'mempty' when the element cannot be found.
--
-- > m ! k == findWithDefault mempty k m
(!) :: (Ord k, Monoid a) => Map k a -> k -> a
(!) = fromMaybe mempty .: (!?)

-- | /O(log n)/. Lookup the value at a key, with a default if the key is missing.
findWithDefault :: (Ord k) => a -> k -> Map k a -> a
findWithDefault a = fromMaybe a .: lookup

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: (Ord k) => k -> Map k a -> Bool
member k (Map m) = Map.member k m

-- | /O(log n)/. Is the key missing from the map? See also 'member'.
notMember :: (Ord k) => k -> Map k a -> Bool
notMember k (Map m) = Map.notMember k m

-- | Find the next smallest key to the given one, and return its key/value pair.
lookupLT :: (Ord k) => k -> Map k a -> Maybe (k, a)
lookupLT k (Map m) = Map.lookupLT k m

-- | Find the next largest key to the given one, and return its key/value pair.
lookupGT :: (Ord k) => k -> Map k a -> Maybe (k, a)
lookupGT k (Map m) = Map.lookupGT k m

-- | Find the largest key up to the given one, and return its key/value pair.
lookupLE :: (Ord k) => k -> Map k a -> Maybe (k, a)
lookupLE k (Map m) = Map.lookupLE k m

-- | Find the smallest key down to the given one, and return its key/value pair.
lookupGE :: (Ord k) => k -> Map k a -> Maybe (k, a)
lookupGE k (Map m) = Map.lookupGE k m

-- | /O(1)/. Is the map empty?
null :: Map k a -> Bool
null (Map m) = Map.null m

-- | /O(1)/. The number of elements in the map.
size :: Map k a -> Int
size (Map m) = Map.size m

-- | /O(m log(n \/ m + 1)), m <= n/. Join two maps together using '(<>)' to combine
--   the values of duplicate keys.
--
--   To retain 'Data.Map'\'s left-biased functionality, use @'unionWith' 'const'@.
union :: (Ord k, Semigroup a) => Map k a -> Map k a -> Map k a
union = (<>)

-- | /O(m log(n \/ m + 1)), m <= n/. Join two maps with a combining function.
unionWith :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f (Map m) (Map n) = Map $ Map.unionWith f m n

-- | /O(m log(n \/ m + 1)), m <= n/. Join two maps with a combining function.
unionWithKey :: (Ord k) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f (Map m) (Map n) = Map $ Map.unionWithKey f m n

-- | A synonym for 'fold'. To retain 'Data.Map'\'s functionality, use @'unionsWith' 'const'@.
unions :: (Foldable t, Ord k, Semigroup a) => t (Map k a) -> Map k a
unions = fold

-- | The union of a list of maps, combining with a specified operation.
unionsWith :: (Foldable t, Ord k) => (a -> a -> a) -> t (Map k a) -> Map k a
unionsWith f = foldl (unionWith f) empty

-- | /O(m log(n \/ m + 1)), m <= n/. The set-difference of the keys in a map,
--   keeping the values of the left-hand map.
difference :: (Ord k) => Map k a -> Map k b -> Map k a
difference (Map m) (Map n) = Map $ Map.difference m n

-- | Infix synonym for 'difference'.
(\\) :: (Ord k) => Map k a -> Map k b -> Map k a
(\\) = difference

-- | /O(n + m)/. Difference with a combining function. Deletes keys if the value is 'Nothing'.
differenceWith :: (Ord k) => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f (Map m) (Map n) = Map $ Map.differenceWith f m n

-- | /O(n + m)/. Difference with a combining function. Deletes keys if the value is 'Nothing'.
differenceWithKey :: (Ord k) => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f (Map m) (Map n) = Map $ Map.differenceWithKey f m n

-- | /O(m log(n \/ m + 1)), m <= n/. The set-intersection of the keys in a map,
--   keeping the values of the left-hand map.
intersection :: (Ord k) => Map k a -> Map k b -> Map k a
intersection (Map m) (Map n) = Map $ Map.intersection m n

-- | /O(m log(n \/ m + 1)), m <= n/. Intersection with a combining function.
intersectionWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f (Map m) (Map n) = Map $ Map.intersectionWith f m n

-- | /O(m log(n \/ m + 1)), m <= n/. Intersection with a combining function.
intersectionWithKey :: (Ord k) => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey f (Map m) (Map n) = Map $ Map.intersectionWithKey f m n

-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> Map k a -> Map k b
map = fmap

-- | /O(n)/. Like 'map' but the function has access to the keys.
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map m) = Map $ Map.mapWithKey f m

-- | /O(n)/. Like 'traverse' but the function has access to the keys.
traverseWithKey :: (Applicative f) => (k -> a -> f b) -> Map k a -> f (Map k b)
traverseWithKey f (Map m) = Map <$> Map.traverseWithKey f m

-- | /O(n)/. Traverse keys/values and collect the 'Just' results.
traverseMaybeWithKey :: (Applicative f) => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseMaybeWithKey f (Map m) = Map <$> Map.traverseMaybeWithKey f m

-- | /O(n)/. Thread an accumulating argument through the map in ascending order of keys.
mapAccum :: (acc -> a -> (acc, b)) -> acc -> Map k a -> (acc, Map k b)
mapAccum f a (Map m) = Map <$> Map.mapAccum f a m

-- | /O(n)/. Thread an accumulating argument through the map in descending order of keys.
mapAccumR :: (acc -> a -> (acc, b)) -> acc -> Map k a -> (acc, Map k b)
mapAccumR f a (Map m) = Map <$> Map.mapAccumRWithKey (const . f) a m

-- | /O(n)/. Like 'mapAccum' but the function has access to the keys.
mapAccumWithKey :: (acc -> k -> a -> (acc, b)) -> acc -> Map k a -> (acc, Map k b)
mapAccumWithKey f acc (Map m) = Map <$> Map.mapAccumWithKey f acc m

-- | /O(n)/. Like 'mapAccumR' but the function has access to the keys.
mapAccumRWithKey :: (acc -> k -> a -> (acc, b)) -> acc -> Map k a -> (acc, Map k b)
mapAccumRWithKey f acc (Map m) = Map <$> Map.mapAccumRWithKey f acc m

-- | /O(n log n)/. Map a function over the keys of the map.
--   If the function maps two or more @j@-keys to the same @k@-key,
--   the values of the @j@-keys are combined with '(<>)'.
--
--   To retain 'Data.Map'\'s greatest-biased functionality, use @'mapKeysWith' 'const'@.
mapKeys :: (Ord k, Semigroup a) => (j -> k) -> Map j a -> Map k a
mapKeys f (Map m) = Map $ Map.mapKeysWith (<>) f m

-- | /O(n log n)/. Map a function over the keys of the map with a value-combining function.
mapKeysWith :: (Ord k) => (a -> a -> a) -> (j -> k) -> Map j a -> Map k a
mapKeysWith c f (Map m) = Map $ Map.mapKeysWith c f m

-- | /O(n)/. Map a strictly-monotonic function over the keys of the map.
mapKeysMonotonic :: (j -> k) -> Map j a -> Map k a
mapKeysMonotonic f (Map m) = Map $ Map.mapKeysMonotonic f m

-- | /O(n log n)/. Map an applicative function over the keys of the map and collect the results.
traverseKeys :: (Applicative f, Ord k, Semigroup a) => (j -> f k) -> Map j a -> f (Map k a)
traverseKeys f = fmap fromList . traverse (\(j, a) -> (,a) <$> f j) . assocs

-- | /O(n log n)/. Map an applicative function over the keys of the map
--   and collect the results using the specified combining function.
traverseKeysWith :: (Applicative f, Ord k) => (a -> a -> a) -> (j -> f k) -> Map j a -> f (Map k a)
traverseKeysWith c f = fmap (fromListWith c) . traverse (\(j, a) -> (,a) <$> f j) . assocs

-- | /O(n)/. Fold the keys and values in the map using the given right-associative binary operator.
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z (Map m) = Map.foldrWithKey f z m

-- | /O(n)/. Fold the keys and values in the map using the given left-associative binary operator.
foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey f z (Map m) = Map.foldlWithKey f z m

-- | /O(n)/. Fold the keys and values in the map strictly,
--   using the given right-associative binary operator.
foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z (Map m) = Map.foldrWithKey' f z m

-- | /O(n)/. Fold the keys and values in the map strictly,
--   using the given left-associative binary operator.
foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' f z (Map m) = Map.foldlWithKey' f z m

-- | /O(n)/. Fold the keys and values in the map using the given monoid.
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
foldMapWithKey f (Map m) = Map.foldMapWithKey f m

-- | /O(n)/. Fold the keys and values in the map strictly using the given monoid.
foldMapWithKey' :: Monoid m => (k -> a -> m) -> Map k a -> m
foldMapWithKey' f (Map m) = Map.foldMapWithKey (f $!) m

-- | /O(n)/. Convert the map to a list of key/value pairs where the keys are in ascending order.
--   Subject to list fusion.
toAscList :: Map k a -> [(k, a)]
toAscList (Map m) = Map.toAscList m

-- | /O(n)/. Convert the map to a list of key/value pairs where the keys are in descending order.
--   Subject to list fusion.
toDescList :: Map k a -> [(k, a)]
toDescList (Map m) = Map.toDescList m

-- | /O(n)/. Filter all values that satisfy the predicate.
filter :: (a -> Bool) -> Map k a -> Map k a
filter p (Map m) = Map $ Map.filter p m

-- | /O(n)/. Filter all key/value pairs that satisfy the predicate.
filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey p (Map m) = Map $ Map.filterWithKey p m

-- | /O(m log(n \/ m + 1)), m <= n/. Restrict a 'Map' to only the keys in a given 'Set'.
restrictKeys :: Ord k => Map k a -> Set k -> Map k a
restrictKeys (Map m) s = Map $ Map.restrictKeys m s

-- | /O(m log(n \/ m + 1)), m <= n/. Remove all the keys in a 'Set' from a 'Map'.
withoutKeys :: Ord k => Map k a -> Set k -> Map k a
withoutKeys (Map m) s = Map $ Map.withoutKeys m s

-- | /O(n)/. Partition the map according to a predicate (satisfied, failed).
partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition p (Map m) = (Map *** Map) $ Map.partition p m

-- | /O(n)/. Partition the map according to a predicate (satisfied, failed).
partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p (Map m) = (Map *** Map) $ Map.partitionWithKey p m

-- | /O(n)/. Take while a predicate on the keys holds. See the note at 'spanAntitone'.
takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone p (Map m) = Map $ Map.takeWhileAntitone p m

-- | /O(n)/. Drop while a predicate on the keys holds. See the note at 'spanAntitone'.
dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone p (Map m) = Map $ Map.dropWhileAntitone p m

-- | /O(n)/. Take while a predicate on the keys holds.
--
--   __NOTE:__ if p is not actually antitone, then 'spanAntitone' will split the map
--   at some unspecified point where the predicate switches from holding to not holding
--   (where the predicate is seen to hold before the first key and to fail after the last key).
spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
spanAntitone p (Map m) = bimap Map Map $ Map.spanAntitone p m

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f (Map m) = Map $ Map.mapMaybe f m

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f (Map m) = Map $ Map.mapMaybeWithKey f m

-- | /O(n)/. Map values and collect the 'Left' and 'Right' results separately.
mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f (Map m) = bimap Map Map $ Map.mapEither f m

-- | /O(n)/. Map values and collect the 'Left' and 'Right' results separately.
mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f (Map m) = bimap Map Map $ Map.mapEitherWithKey f m

-- | /O(log n)/. Partition the map by comparing keys ((smaller, larger) than given).
split :: (Ord k) => k -> Map k a -> (Map k a, Map k a)
split k (Map m) = bimap Map Map $ Map.split k m

-- | /O(log n)/. Like 'split' but the middle coordinate 'lookup's the value at the key.
splitLookup :: (Ord k) => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup k (Map m) = (\(n, y, p) -> (Map n, y, Map p)) $ Map.splitLookup k m

-- | /O(1)/. Decompose a map into pieces based on the structure of the underlying tree.
splitRoot :: Map k a -> [Map k a]
splitRoot (Map m) = Map <$> Map.splitRoot m

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ their values all agree.
isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf (Map m) (Map n) = Map.isSubmapOf m n

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ the function returns 'True' when applied to respective values.
isSubmapOfBy :: (Ord k) => (a -> a -> Bool) -> Map k a -> Map k a -> Bool
isSubmapOfBy f (Map m) (Map n) = Map.isSubmapOfBy f m n

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ their values all agree, __and__ the maps are not equal.
isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf (Map m) (Map n) = Map.isProperSubmapOf m n

-- | /O(m log(n \/ m + 1)), m <= n/. Returns 'True' if all the keys in the left map
--   exist in the right, __and__ the function returns 'True' when applied to respective values,
--   __and__ the maps are not equal.
isProperSubmapOfBy :: (Ord k) => (a -> a -> Bool) -> Map k a -> Map k a -> Bool
isProperSubmapOfBy f (Map m) (Map n) = Map.isProperSubmapOfBy f m n

-- | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index
--   in the ordered sequence of keys.
--
-- > 'lookupIndex' k m == 'Data.List.findIndex' k ('keys' m)
lookupIndex :: (Ord k) => k -> Map k a -> Maybe Int
lookupIndex k (Map m) = Map.lookupIndex k m

-- | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index
--   in the ordered sequence of keys. Calls 'error' when the key is not in the map.
findIndex :: (Ord k) => k -> Map k a -> Int
findIndex i (Map m) = Map.findIndex i m

-- | /O(log n)/. Retrieve an element by its /index/. Calls 'error' if @i@ is outside
--   the range @0 <= i < 'size' m@.
elemAt :: (Ord k) => Int -> Map k a -> (k, a)
elemAt i (Map m) = Map.elemAt i m

-- | /O(log n)/. Update the element by its /index/. Calls 'error' if @i@ is outside
--   the range @0 <= i < 'size' m@.
updateAt :: (Ord k) => (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt f i (Map m) = Map $ Map.updateAt f i m

-- | /O(log n)/. Delete the element by its /index/. Calls 'error' if @i@ is outside
--   the range @0 <= i < 'size' m@.
deleteAt :: (Ord k) => (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
deleteAt f i (Map m) = Map $ Map.updateAt f i m

-- | Take the smallest @n@ keys.
take :: Int -> Map k a -> Map k a
take n (Map m) = Map $ Map.take n m

-- | Drop the smallest @n@ keys.
drop :: Int -> Map k a -> Map k a
drop n (Map m) = Map $ Map.take n m

-- | /O(n)/. Split a map at a particular index.
splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt i (Map m) = bimap Map Map $ Map.splitAt i m

-- | /O(log n)/. The minimal key of the map, or 'Nothing' if the map is empty.
lookupMin :: Map k a -> Maybe (k, a)
lookupMin (Map m) = Map.lookupMin m

-- | /O(log n)/. The maximal key of the map, or 'Nothing' if the map is empty.
lookupMax :: Map k a -> Maybe (k, a)
lookupMax (Map m) = Map.lookupMax m

-- | /O(log n)/. The minimal key of the map, or 'error' if the map is empty.
findMin :: Map k a -> (k, a)
findMin (Map m) = Map.findMin m

-- | /O(log n)/. The maximal key of the map, or 'error' if the map is empty.
findMax :: Map k a -> (k, a)
findMax (Map m) = Map.findMax m

-- | /O(log n)/. Delete the minimal key.
deleteMin :: Map k a -> Map k a
deleteMin (Map m) = Map $ Map.deleteMin m

-- | /O(log n)/. Delete the maximal key.
deleteMax :: Map k a -> Map k a
deleteMax (Map m) = Map $ Map.deleteMax m

-- | /O(log n)/. Delete and return the minimal key of the map, or 'error' if the map is empty.
deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin (Map m) = Map <$> Map.deleteFindMin m

-- | /O(log n)/. Delete and return the maximal key of the map, or 'error' if the map is empty.
deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax (Map m) = Map <$> Map.deleteFindMax m

-- | /O(log n)/. Update the value at the minimal key.
updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f (Map m) = Map $ Map.updateMin f m

-- | /O(log n)/. Update the value at the maximal key.
updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f (Map m) = Map $ Map.updateMax f m

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey f (Map m) = Map $ Map.updateMinWithKey f m

-- | /O(log n)/. Update the value at the maximal key.
updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey f (Map m) = Map $ Map.updateMaxWithKey f m

-- | /O(log n)/. Retrieve the value associated with the minimal key of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
minView :: Map k a -> Maybe (a, Map k a)
minView (Map m) = fmap Map <$> Map.minView m

-- | /O(log n)/. Retrieve the value associated with the maximal key of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
maxView :: Map k a -> Maybe (a, Map k a)
maxView (Map m) = fmap Map <$> Map.maxView m

-- | /O(log n)/. Retrieve the minimal key/value pair of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey (Map m) = fmap Map <$> Map.minViewWithKey m

-- | /O(log n)/. Retrieve the maximal key/value pair of the map,
--   and the map stripped of that element, or 'Nothing' if passed an empty map.
maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey (Map m) = fmap Map <$> Map.maxViewWithKey m

-- | "Transpose" a nested map, by swapping the outer two "dimensions".
invertKeys :: (Ord j, Ord k, Semigroup a) => Map j (Map k a) -> Map k (Map j a)
invertKeys (assocs -> jkas) = fromList [(k, singleton j a) | (j, m) <- jkas, (k, a) <- assocs m]
