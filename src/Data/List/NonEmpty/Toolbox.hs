-- |
-- Module       : Data.List.NonEmpty.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of @Data.List.NonEmpty@.
--
-- This module re-exports the above library, so modules need only import @Data.List.NonEmpty.Toolbox@.
module Data.List.NonEmpty.Toolbox (
    -- * Re-exports
    module Data.List.NonEmpty,

    -- * Quasi-constructors
    (|:),
    (|>),
    (<||),
    (||>),

    -- * @Foldable@ specializations
    fold1,
    foldMap1,
    foldMap1',
    maximum1,
    minimum1,
    maximumBy1,
    minimumBy1,
    maximumOn1,
    minimumOn1,
    maximumOf1,
    minimumOf1,

    -- * List functions
    sortOn,
    union,
    unionBy,
    unionOn,
    intersect,
    intersectBy,
) where

import qualified Data.Foldable.Toolbox as FT
import Data.Function.Toolbox (using)
import Data.List.NonEmpty
import qualified Data.List.Toolbox as LT
import Data.Maybe (fromJust)
import Prelude hiding (maximum, minimum)

infixl 5 |:, |>

-- | Append an element to a list.
--
-- > [1, 2, 3] |: 4 == 1 :| [2, 3, 4]
(|:) :: [a] -> a -> NonEmpty a
(|:) = flip (foldr cons . pure)

-- | Append an element to a @NonEmpty@ list.
--
-- > (1 :| [2, 3]) |> 4 == 1 :| [2, 3, 4]
(|>) :: NonEmpty a -> a -> NonEmpty a
xs |> x = xs <> pure x

-- | Append a list to a @NonEmpty@ list.
--
-- (1 :| [2, 3]) ||> [4, 5] == 1 :| [2, 3, 4, 5]
(||>) :: NonEmpty a -> [a] -> NonEmpty a
(x :| xs) ||> lx = x :| xs <> lx

-- | Append a @NonEmpty@ list to a list.
--
-- > [1, 2, 3] <|| (4 :| [5]) == 1 :| [2, 3, 4, 5]
(<||) :: [a] -> NonEmpty a -> NonEmpty a
(<||) = flip (foldr cons)

-- | A specialized version of @fold1@ to @NonEmpty@ lists.
fold1 :: (Semigroup m) => NonEmpty m -> m
fold1 = fromJust . FT.fold1

-- | A specialized version of @foldMap1@ to @NonEmpty@ lists.
foldMap1 :: (Semigroup m) => (a -> m) -> NonEmpty a -> m
foldMap1 f = fromJust . FT.foldMap1 f

-- | A specialized version of @foldMap1'@ to @NonEmpty@ lists.
foldMap1' :: (Semigroup m) => (a -> m) -> NonEmpty a -> m
foldMap1' f = fromJust . FT.foldMap1' f

-- | A specialized version of @maximum@ to @NonEmpty@ lists.
--
-- > maximum1 (1 :| [2, 3, 4]) == 4
maximum1 :: (Ord a) => NonEmpty a -> a
maximum1 = fromJust . FT.maximumOn id

-- | A specialized version of @minimum@ to @NonEmpty@ lists.
--
-- > minimum1 (1 :| [2, 3, 4]) == 1
minimum1 :: (Ord a) => NonEmpty a -> a
minimum1 = fromJust . FT.minimumOn id

-- | A specialized version of @maximumBy@ to @NonEmpty@ lists.
--
-- > maximumBy1 (comparing negate) (1 :| [2, 3, 4]) == 1
maximumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy1 = FT.maximumBy

-- | A specialized version of @minimumBy@ to @NonEmpty@ lists.
--
-- > minimumBy1 (comparing negate) (1 :| [2, 3, 4]) == 4
minimumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy1 = FT.minimumBy

-- | A specialized version of @maximumOn@ to @NonEmpty@ lists.
--
-- > maximumOn1 negate (1 :| [2, 3, 4]) == -1
maximumOn1 :: (Ord b) => (a -> b) -> NonEmpty a -> a
maximumOn1 f = fromJust . FT.maximumOn f

-- | A specialized version of @minimumOn@ to @NonEmpty@ lists.
--
-- > minimumOn1 negate (1 :| [2, 3, 4]) == -4
minimumOn1 :: (Ord b) => (a -> b) -> NonEmpty a -> a
minimumOn1 f = fromJust . FT.minimumOn f

-- | A specialized version of @maximumOf@ to @NonEmpty@ lists.
--
-- > maximumOf1 negate (1 :| [2, 3, 4]) == -1
maximumOf1 :: (Ord b) => (a -> b) -> NonEmpty a -> b
maximumOf1 f = fromJust . FT.maximumOf f

-- | A specialized version of @minimumOf@ to @NonEmpty@ lists.
--
-- > minimumOf1 negate (1 :| [2, 3, 4]) == -4
minimumOf1 :: (Ord b) => (a -> b) -> NonEmpty a -> b
minimumOf1 f = fromJust . FT.minimumOf f

-- | A version of @Data.List.sortOn@ for @NonEmpty@ lists.
sortOn :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = fromList . LT.sortOn f . FT.toList

-- | A version of @Data.List.union@ for @NonEmpty@ lists.
union :: (Eq a) => NonEmpty a -> NonEmpty a -> NonEmpty a
union = unionBy (==)

-- | A version of @Data.List.unionBy@ for @NonEmpty@ lists.
unionBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionBy f = (fromList .) . (LT.unionBy f `using` toList)

-- | A version of @Data.List.unionBy@ for @NonEmpty@ lists.
unionOn :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionOn f = unionBy ((==) `using` f)

-- | A version of @Data.List.intersect@ for @NonEmpty@ lists.
--
--   Note that the @NonEmpty@ guarantee is not preserved.
--
-- > intersect (1 :| [2, 3]) (2 :| [3, 4]) == [2, 3]
-- > intersect (1 :| [2, 3]) (4 :| [5, 6]) == []
intersect :: (Eq a) => NonEmpty a -> NonEmpty a -> [a]
intersect = LT.intersect `using` toList

-- | A version of @Data.List.intersectBy@ for @NonEmpty@ lists.
--
--   Note that the @NonEmpty@ guarantee is not preserved.
intersectBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> [a]
intersectBy f = LT.intersectBy f `using` toList
