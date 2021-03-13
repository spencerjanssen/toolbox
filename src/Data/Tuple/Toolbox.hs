-- |
-- Module       : Data.Tuple.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Tuple'.
--
-- This module re-exports the above library, so modules need only import 'Data.Tuple.Toolbox'.
module Data.Tuple.Toolbox (
    -- * Pairs
    (&&&),
    (***),
    both,
    assoc,
    unassoc,

    -- * Triples
    fst3,
    snd3,
    thd3,
    first3,
    second3,
    third3,

    -- * 4-tuples
    frst4,
    scnd4,
    thrd4,
    frth4,
    first4,
    second4,
    third4,
    fourth4,

    -- * Re-exports
    module Data.Tuple,
) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Tuple

infix 1 &&&, ***

-- | Apply two functions to the same value.
--
-- > (pred &&& succ) 0 == (-1, 1)
(&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
(&&&) = liftA2 (,)

-- | Apply two functions pairwise.
--
-- > (pred *** succ) (0, 0) == (-1, 1)
(***) :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
(***) = bimap

-- | Apply the same function to both components.
both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

-- | Associate a pair-inside-a-pair.
--
-- > assoc . unassoc == id
assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

-- | Un-associate a pair-inside-a-pair.
--
-- > unassoc . assoc == id
unassoc :: ((a, b), c) -> (a, (b, c))
unassoc ((a, b), c) = (a, (b, c))

-- | Get the first component of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Get the second component of a triple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Get the third component of a triple.
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Map a function over the first component of a triple.
--
-- > first3 succ (0, 0, 0) == (1, 0, 0)
first3 :: (a -> x) -> (a, b, c) -> (x, b, c)
first3 f (a, b, c) = (f a, b, c)

-- | Map a function over the second component of a triple.
--
-- > second3 succ (0, 0, 0) == (0, 1, 0)
second3 :: (b -> x) -> (a, b, c) -> (a, x, c)
second3 f (a, b, c) = (a, f b, c)

-- | Map a function over the third component of a triple.
--
-- > third3 succ (0, 0, 0) == (0, 0, 1)
third3 :: (c -> x) -> (a, b, c) -> (a, b, x)
third3 f (a, b, c) = (a, b, f c)

-- | Get the first component of a 4-tuple.
frst4 :: (a, b, c, d) -> a
frst4 (a, _, _, _) = a

-- | Get the second component of a 4-tuple.
scnd4 :: (a, b, c, d) -> b
scnd4 (_, b, _, _) = b

-- | Get the third component of a 4-tuple.
thrd4 :: (a, b, c, d) -> c
thrd4 (_, _, c, _) = c

-- | Get the fourth component of a 4-tuple.
frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, d) = d

-- | Map a function over the first component of a 4-tuple.
--
-- > first4 succ (0, 0, 0, 0) == (1, 0, 0, 0)
first4 :: (a -> x) -> (a, b, c, d) -> (x, b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

-- | Map a function over the second component of a 4-tuple.
--
-- > second4 succ (0, 0, 0, 0) == (0, 1, 0, 0)
second4 :: (b -> x) -> (a, b, c, d) -> (a, x, c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

-- | Map a function over the third component of a 4-tuple.
--
-- > third4 succ (0, 0, 0, 0) == (0, 0, 1, 0)
third4 :: (c -> x) -> (a, b, c, d) -> (a, b, x, d)
third4 f (a, b, c, d) = (a, b, f c, d)

-- | Map a function over the fourth component of a 4-tuple.
--
-- > fourth4 succ (0, 0, 0, 0) == (0, 0, 0, 1)
fourth4 :: (d -> x) -> (a, b, c, d) -> (a, b, c, x)
fourth4 f (a, b, c, d) = (a, b, c, f d)
