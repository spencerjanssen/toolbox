{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Module       : Data.Function.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of @Data.Function@.
--
-- This module re-exports the above library, so modules need only import @Data.Function.Toolbox@.
module Data.Function.Toolbox (
    -- * Re-exports
    module Data.Function,

    -- * More curries and uncurries
    curry3,
    curry4,
    curry5,
    curry6,
    curry7,
    curry8,
    curry9,
    curry10,
    curry11,
    curry12,
    curry13,
    curry14,
    curry15,
    curry16,
    curry17,
    curry18,
    curry19,
    curry20,
    uncurry3,
    uncurry4,
    uncurry5,
    uncurry6,
    uncurry7,
    uncurry8,
    uncurry9,
    uncurry10,
    uncurry11,
    uncurry12,
    uncurry13,
    uncurry14,
    uncurry15,
    uncurry16,
    uncurry17,
    uncurry18,
    uncurry19,
    uncurry20,

    -- * Extra utilities
    applyN,
    applyWhen,
    (.:),
    using,
) where

import Data.Function
import Data.Function.Toolbox.Internal (mkCurryN, mkUncurryN)
import Data.Semigroup (Endo (..), stimesMonoid)

-- | Apply a function @n@ times.
--
-- > \x -> applyN 5 succ x == x + 5
applyN :: (Integral n) => n -> (a -> a) -> (a -> a)
applyN n = appEndo . stimesMonoid n . Endo

-- | Apply a function if the condition succeeds.
--
-- > \x -> applyWhen True succ x == x + 1
-- > \b x -> applyWhen b succ x == applyN (fromEnum b) succ x
applyWhen :: Bool -> (a -> a) -> (a -> a)
applyWhen b f = if b then f else id

-- | Apply a single-argument function after a two-argument function.
--
-- > \x y -> (succ .: (+)) x y == (x + y) + 1
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .: f = (g .) . f

-- | Like @on@, but memoizes values to minimize necessary calculations.
using :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
cmp `using` f = \x -> let fx = f x in cmp fx . f

$(mkCurryN 20)
$(mkUncurryN 20)
