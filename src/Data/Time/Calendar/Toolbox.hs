{-# LANGUAGE ViewPatterns #-}

-- |
-- Module       : Data.Time.Calendar.Toolbox
-- Copyright    : (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of @Data.Time.Calendar@.
--
-- This module re-exports the above module, so modules need only import @Data.Time.Calendar.Toolbox@.
module Data.Time.Calendar.Toolbox (
    -- * Re-exports
    module Data.Time.Calendar,

    -- * The 'Month' datatype
    Month (..),
    mkGregorian,
    unGregorian,
    monthLength,
    nextYearMonth,
    lastYearMonth,

    -- * Convenience functions
    firstOfMonth,
    lastOfMonth,
    firstOfYear,
    lastOfYear,
    monthAgo,
    monthFromNow,
) where

import Data.Ix (Ix)
import Data.Time.Calendar
import Data.Tuple.Toolbox (second3)

-- | Months of the year.
data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Eq, Ord, Show, Read, Ix)

-- | "Circular", so for example @[March ..]@ gives an endless sequence.
--   Also 'fromEnum' gives @[1 .. 12]@ for @[January .. December]@, and
--   'toEnum' performs mod 12 to give a cycle of months.
instance Enum Month where
    toEnum n = case n `mod` 12 of
        1 -> January
        2 -> February
        3 -> March
        4 -> April
        5 -> May
        6 -> June
        7 -> July
        8 -> August
        9 -> September
        10 -> October
        11 -> November
        _ -> December
    fromEnum m = case m of
        January -> 1
        February -> 2
        March -> 3
        April -> 4
        May -> 5
        June -> 6
        July -> 7
        August -> 8
        September -> 9
        October -> 10
        November -> 11
        December -> 12
    enumFromTo m1 m2 = if m1 == m2 then [m1] else m1 : enumFromTo (succ m1) m2
    enumFromThenTo m1 m2 m3 =
        if m2 == m3
            then [m1, m2]
            else m1 : enumFromThenTo m2 (toEnum $ 2 * fromEnum m2 - fromEnum m1) m3

-- | Like 'toGregorian' but with a 'Month'.
mkGregorian :: Day -> (Integer, Month, Int)
mkGregorian = second3 toEnum . toGregorian

-- | Like 'fromGregorian' but with a 'Month'.
unGregorian :: Integer -> Month -> Int -> Day
unGregorian = (. fromEnum) . fromGregorian

-- | The length of the 'Month' in the given year.
monthLength :: Integer -> Month -> Int
monthLength = (. fromEnum) . gregorianMonthLength

-- | Like 'succ' but increments the year on rollover.
nextYearMonth :: (Integer, Month) -> (Integer, Month)
nextYearMonth (y, m) = case m of
    December -> (succ y, January)
    _ -> (y, succ m)

-- | Like 'pred' but decrements the year on rollover.
lastYearMonth :: (Integer, Month) -> (Integer, Month)
lastYearMonth (y, m) = case m of
    January -> (pred y, December)
    _ -> (y, pred m)

-- | Get the first day of the given 'Day'\'s month.
firstOfMonth :: Day -> Day
firstOfMonth (toGregorian -> (y, m, _)) = fromGregorian y m 1

-- | Get the last day of the given 'Day'\'s month.
lastOfMonth :: Day -> Day
lastOfMonth (toGregorian -> (y, m, _)) = fromGregorian y m $ gregorianMonthLength y m

-- | Get the first day of the given 'Day'\'s year.
firstOfYear :: Day -> Day
firstOfYear (toGregorian -> (y, _, _)) = fromGregorian y 1 1

-- | Get the last day of the given 'Day'\'s year.
lastOfYear :: Day -> Day
lastOfYear (toGregorian -> (y, _, _)) = fromGregorian y 12 31

-- | Synonym for @'addGregorianMonthsClip' (-1)@.
--
-- > monthAgo (read "1993-09-01") == read "1993-08-01"
monthAgo :: Day -> Day
monthAgo = addGregorianMonthsClip (-1)

-- | Synonym for @'addGregorianMonthsClip' 1@.
--
-- > monthFromNow (read "1993-08-31") == read "1993-09-30"
monthFromNow :: Day -> Day
monthFromNow = addGregorianMonthsClip 1
