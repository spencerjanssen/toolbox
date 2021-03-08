-- |
-- Module       : Data.Foldable.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of @Data.Foldable@.
--
-- This module re-exports the above library, so modules need only import @Data.Foldable.Toolbox@.
module Data.Foldable.Toolbox (
    -- * Re-exports
    module Data.Foldable,

    -- * General utilities
    notNull,
    genericLength,
    foldMap1,
    foldMap1',
    firstJust,
    sumOn,
    productOn,
    maximumOn,
    minimumOn,

    -- * Monadic utilities
    foldMapM,
    anyM,
    allM,
    orM,
    andM,
    findM,
    firstJustM,
) where

import Control.Monad.Toolbox (ifM, maybeM, (&^&), (|^|))
import Data.Coerce (coerce)
import Data.Foldable
import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Max (..), Min (..), Product (..), Sum (..))

-- | The negation of @null@.
notNull :: (Foldable t) => t a -> Bool
notNull = foldr (\_ _ -> True) False

-- | A version of @length@ where the result can be of any @Integral@ type.
genericLength :: (Foldable t, Integral n) => t a -> n
genericLength = fromIntegral . length

-- | A version of @foldMap@ that works on a @Semigroup@ instead of a @Monoid@.
foldMap1 :: (Foldable t, Semigroup m) => (a -> m) -> t a -> Maybe m
foldMap1 f = foldMap (Just . f)

-- | A version of @foldMap'@ that works on a @Semigroup@ instead of a @Monoid@.
foldMap1' :: (Foldable t, Semigroup m) => (a -> m) -> t a -> Maybe m
foldMap1' f = foldMap' (Just . f)

-- | Map each element of the structure monadically to a monoid, and combine the results underneath.
--
-- > foldMapM Just [Sum 2, Sum 5] == Just (Sum 7)
foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\mb a -> (<> mb) <$> f a) mempty
{-# INLINE foldMapM #-}

-- | A version of @any@, where the predicate can be monadic. Short-circuits.
--
-- > anyM (\a -> Just (a > 3)) [1, 4, undefined] == Just True
anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldr ((|^|) . f) (pure False)

-- | A version of @all@, where the predicate can be monadic. Short-circuits.
--
-- > allM (\a -> Just (a < 3)) [1, 4, undefined] == Just False
allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = foldr ((&^&) . f) (pure True)

-- | A version of @or@, where the values can be monadic. Short-circuits.
--
-- > orM [Just True, Just False, undefined] == Just True
orM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
orM = anyM id

-- | A version of @and@, where the values can be monadic. Short-circuits.
--
-- > andM [Just True, Just False, undefined] == Just False
andM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
andM = allM id

-- | A version of @find@ where the predicate can be monadic.
--
-- > findM (\a -> [a `mod` 7 == 0]) [10, 9 .. 1] == [Just 7]
findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr (ifM <$> p <*> (pure . Just)) (pure Nothing)

-- | A version of @firstJust@ where the predicate can be monadic.
firstJustM :: (Foldable t, Monad m) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
firstJustM f = foldl' (\mmb a -> maybeM (f a) (pure . Just) mmb) (pure Nothing)

-- | Find the first @Just@ result of applying a function to each element of the structure.
--
-- > firstJust listToMaybe [[], [], [5, 6, 7]] == Just 5
firstJust :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
firstJust f = runIdentity . firstJustM (Identity . f)

-- | Extract a numerical value from each element of the structure and sum them all.
--
-- > sumOn read ["4", "2", "3"] == 9
-- > sumOn read [] == 0
sumOn :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumOn f = coerce . foldMap' (coerce f `asTypeOf` (Sum . f))

-- | Extract a numerical value from each element of the structure and multiply them all.
--
-- > productOn read ["4", "2", "3"] == 24
-- > productOn read [] == 1
productOn :: (Foldable t, Num b) => (a -> b) -> t a -> b
productOn f = coerce . foldMap' (coerce f `asTypeOf` (Product . f))

-- | Find the maximum value attained by the function on the structure.
--
-- > maximumOn read ["4", "2", "3"] == Just 4
-- > maximumOn read [] == Nothing
maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe b
maximumOn f = coerce . foldMap1' (coerce f `asTypeOf` (Max . f))

-- | Find the minimum value attained by the function on the structure.
--
-- > minimumOn read ["4", "2", "3"] == Just 2
-- > minimumOn read [] == Nothing
minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe b
minimumOn f = coerce . foldMap1' (coerce f `asTypeOf` (Min . f))
