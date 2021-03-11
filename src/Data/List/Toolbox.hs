-- |
-- Module       : Data.List.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of @Data.List@.
--
-- This module re-exports the above module, as well as @Data.Foldable.Toolbox@,
-- so modules need only import @Data.List.Toolbox@.
module Data.List.Toolbox (
    -- * Re-exports
    module Data.Foldable.Toolbox,
    module Data.List,

    -- * Basic combinators
    list,
    safeHead,
    safeLast,
    safeTail,
    safeInit,
    tuple2,
    tuple3,
    tuple4,
    (!?),
    enumerate,
    chopInfix,
    removed,
    replace,
    replaceFirst,
    splitOn,
    chunksOf,
    genericChunksOf,

    -- * Predicates over lists
    takeEnd,
    genericTakeEnd,
    dropEnd,
    genericDropEnd,
    takeWhileEnd,
    spanEnd,
    breakEnd,

    -- * Predicates about lists
    anySame,
    anySameOrd,
    allSame,
    disjoint,
    disjointOrd,

    -- * Ordered lists
    merge,
    mergeBy,
    mergeOn,
    groupOn,
    groupSort,
    groupSortOn,
    intersectOn,
    unionOn,
) where

import Data.Bifunctor (first, second)
import Data.Foldable.Toolbox
import Data.Function.Toolbox (using)
import Data.List hiding (genericLength)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Maybe (fromJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A non-recursive eliminator for @[a]@.
--
-- > \xs -> list Nothing (const . Just) xs == listToMaybe xs
list :: b -> (a -> [a] -> b) -> [a] -> b
list n c lx = case lx of
    [] -> n
    (x : xs) -> c x xs

-- | A convenient synonym for 'listToMaybe'.
safeHead :: [a] -> Maybe a
safeHead = listToMaybe

-- | Get the last element of a (possibly empty) list.
--
-- > safeHast [] == Nothing
-- > safeHast [2, 3, 4] == Just 4
safeLast :: [a] -> Maybe a
safeLast = foldl (const Just) Nothing

-- | A version of 'tail' that does not fail on empty lists.
--
-- > safeTail [2, 3, 4] == [3, 4]
-- > safeTail [] == []
safeTail :: [a] -> [a]
safeTail = list [] (\_ xs -> xs)

-- | A version of 'init' that does not fail on empty lists.
--
-- > safeInit [2, 3, 4] == [2, 3]
-- > safeInit [] == []
safeInit :: [a] -> [a]
safeInit = dropEnd 1

-- | Create a pair from the first two elements of a list, if they exist.
tuple2 :: [a] -> Maybe (a, a)
tuple2 (x : y : _) = Just (x, y)
tuple2 _ = Nothing

-- | Create a triple from the first three elements of a list, if they exist.
tuple3 :: [a] -> Maybe (a, a, a)
tuple3 (x : y : z : _) = Just (x, y, z)
tuple3 _ = Nothing

-- | Create a triple from the first three elements of a list, if they exist.
tuple4 :: [a] -> Maybe (a, a, a, a)
tuple4 (x : y : z : w : _) = Just (x, y, z, w)
tuple4 _ = Nothing

-- | Get the value of a list at an index, if the list is long enough. Safe version of '(!!)'.
--
-- > ['a', 'b', 'c'] !? 2 == Just 'c'
-- > ['a', 'b', 'c'] !? 4 == Nothing
-- > \xs n -> n < 0 ==> xs !? n == Nothing
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
xs !? 0 = safeHead xs
(_ : xs) !? n = if n < 0 then Nothing else xs !? pred n

-- | Check if the list contains duplicates. /O(n^2)/.
--
-- > anySame [2, 3, 4, 3] == True
-- > anySame [2, 5, 6, 7] == False
anySame :: (Eq a) => [a] -> Bool
anySame = go []
  where
    go :: (Eq a) => [a] -> [a] -> Bool
    go seen (x : xs) = x `elem` seen || go (x : seen) xs
    go _ [] = False

-- | /O(n log n)/. Check if the list contains duplicates efficiently.
--
-- > \xs -> anySame xs == anySameOrd xs
anySameOrd :: (Ord a) => [a] -> Bool
anySameOrd = go Set.empty
  where
    go :: (Ord a) => Set a -> [a] -> Bool
    go seen (x : xs) = x `Set.member` seen || go (Set.insert x seen) xs
    go _ [] = False

-- | /O(n)/. Check if all the elements of the list are equal.
--
-- > allSame [2, 2, 2] == True
-- > allSame [2, 3, 4] == False
allSame :: (Eq a) => [a] -> Bool
allSame = list True (all . (==))

-- | Tests if the lists have no elements in common.
--
-- > disjoint "abc" "def" == True
-- > disjoint "" undefined == True
-- > disjoint undefined "" == undefined
-- > disjoint "abc" "cde" == False
disjoint :: (Eq a) => [a] -> [a] -> Bool
disjoint = (null .) . intersect

-- | Like 'disjoint', but with better algorithmic complexity.
disjointOrd :: (Ord a) => [a] -> [a] -> Bool
disjointOrd = Set.disjoint `using` Set.fromList

-- | An ordered list of values of a 'Bounded' 'Enum' type.
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

-- | A version of 'merge' that accepts a user-defined comparison function.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy f (x : xs) (y : ys) = case f x y of
    GT -> y : mergeBy f (x : xs) ys
    _ -> x : mergeBy f xs (y : ys)

-- | Combine two lists, preserving order. Assumes both input lists have been sorted.
--
-- > merge [2, 3, 6] [1, 4, 7] == [1, 2, 3, 4, 6, 7]
merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

-- | A version of 'mergeBy' that accepts a valuation function instead of a comparison.
mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn f = mergeBy (compare `using` f)

-- | A version of 'take' that keeps values from the end of the list.
--
-- > takeEnd 2 [1, 2, 3] == [2, 3]
-- > \xs i -> i >= 0 ==> takeEnd i xs == drop (length xs - i) xs
takeEnd :: Int -> [a] -> [a]
takeEnd n xs = if n <= 0 then [] else go xs (drop n xs)
  where
    go :: [a] -> [a] -> [a]
    go (_ : as) (_ : bs) = go as bs
    go as _ = as

-- | A version of 'drop' that removes values from the end of the list.
--
-- > dropEnd 2 [1, 2, 3] == [1]
-- > \xs i -> i >= 0 ==> dropEnd i xs == take (length xs - i) xs
dropEnd :: Int -> [a] -> [a]
dropEnd n xs = if n <= 0 then xs else zipWith const xs (drop n xs)

-- | A version of 'takeWhile' that keeps values from the end of the list.
--
-- > takeWhileEnd odd [2, 4, 1, 3, 5] == [1, 3, 5]
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

-- | A version of 'takeEnd' where the parameneter can be of any 'Integral' type.
genericTakeEnd :: (Integral n) => n -> [a] -> [a]
genericTakeEnd = takeEnd . fromIntegral

-- | A version of 'dropEnd' where the parameter can be of any 'Integral' type.
genericDropEnd :: Int -> [a] -> [a]
genericDropEnd = dropEnd . fromIntegral

-- | A version of 'span' that runs from the end of the list.
--
-- > spanEnd odd [1, 7, 4, 3, 5] == ([3, 5], [1, 7, 4])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (takeWhileEnd p xs, dropWhileEnd p xs)

-- | A version of 'break' that runs from the end of the list.
--
-- > breakEnd odd [1, 7, 4, 3, 5] == ([1, 7, 4], [3, 5])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)

-- | Like 'removed', but keeps the search string as a prefix.
--
-- > chopInfix (fromList "c") "abcde" == ("ab", "cde")
chopInfix :: (Eq a) => NonEmpty a -> [a] -> ([a], [a])
chopInfix _ [] = ([], [])
chopInfix as lx@(x : xs) =
    if toList as `isPrefixOf` lx
        then ([], lx)
        else first (x :) $ chopInfix as xs

-- | @'removed' as xs@ removes the first occurrence of @as@ from @xs@ by
--   returning the surrounding prefix and suffix.
--
-- > removed "" "abc" == ("", "abc")
-- > removed "c" "abcde" == ("ab", "de")
removed :: (Eq a) => [a] -> [a] -> ([a], [a])
removed [] xs = ([], xs)
removed as xs = second (drop (length as)) $ chopInfix (fromList as) xs

-- | A version of 'replace' that only substitutes the first occurrence.
--
-- > replaceFirst (fromList "z") "" "azbzc" == "abzc"
-- > replaceFirst (fromList "z") "xy" "azbzc" == "axybzc"
replaceFirst :: (Eq a) => NonEmpty a -> [a] -> [a] -> [a]
replaceFirst as bs = (\(pre, post) -> pre ++ bs ++ post) . removed (toList as)

-- | A version of 'groupBy' that accepts a user-defined function on which to test equality.
--
-- > groupOn fst [(1, 3), (1, 4), (2, 4)] == [[(1, 3), (1, 4)], [(2, 4)]]
-- > groupOn snd [(1, 3), (1, 4), (2, 4)] == [[(1, 3)], [(1, 4), (2, 4)]]
groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `using` f)

-- | A composition of 'group' and 'sort'.
--
-- > groupSort [1, 3, 4, 3, 1] == [[1, 1], [3, 3], [4]]
groupSort :: (Ord a) => [a] -> [[a]]
groupSort = group . sort

-- | A version of 'groupSort' that accepts a user-defined comparison function.
--
-- > groupSortOn fst [(1, 3), (2, 4), (1, 4)] == [[(1, 3), (1, 4)], [(2, 4)]]
-- > groupSortOn snd [(1, 3), (2, 4), (1, 4)] == [[(1, 3)], [(2, 4), (1, 4)]]
groupSortOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortOn f = map (map (snd . getFst)) . groupSort . map (mkFst f)

-- | A version of 'intersectBy' that accepts a user-defined function on which to test equality.
--   Prefers elements from the first list.
--
-- > intersectOn abs [-1, -2] [2, 3] == [-2]
-- > intersectOn signum [-1, -2] [2, 3] == []
intersectOn :: (Eq b) => (a -> b) -> [a] -> [a] -> [a]
intersectOn f = intersectBy ((==) `using` f)

-- | A version of 'unionBy' that accepts a user-defined function on which to test equality.
--   Prefers elements from the first list.
--
-- > unionOn abs [-1, -2] [2, 3] == [-1, -2, 3]
-- > unionOn abs [-1] [2, 3] == [-1, 2, 3]
unionOn :: (Eq b) => (a -> b) -> [a] -> [a] -> [a]
unionOn f = unionBy ((==) `using` f)

-- | @'replace' as bs xs@ substitutes the sublist @as@ with @bs@ each place @as@ occurs in @xs@.
--   To replace only the first occurrence, use 'replaceFirst'.
--
-- > replace (fromList "c") "z" "abcdec" == "abzdez"
-- > replace (fromList "c") "a" "vwxyz" == "vwxyz"
replace :: (Eq a) => NonEmpty a -> [a] -> [a] -> [a]
replace _ _ [] = []
replace la bs lx@(x : xs) =
    if as `isPrefixOf` lx
        then bs ++ replace la bs (fromJust (stripPrefix as lx))
        else x : replace la bs xs
  where
    as = toList la

-- | A version of 'replace' where instead of conjoining the prefix and suffix,
--   they are kept as separate sublists.
--
-- > splitOn (fromList "z") "azbzc" == ["a","b","c"]
-- > splitOn (fromList "z") "azbzcz" == ["a","b","c",""]
-- > splitOn (fromList "z") "zazbzc" == ["","a","b","c"]
splitOn :: (Eq a) => NonEmpty a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn as lx =
    pre : case post of
        [] -> []
        _ -> splitOn as $ drop (length as) post
  where
    (pre, post) = chopInfix as lx

-- | Split a list into chunks of a given size.
--
-- > chunksOf 2 "abcde" == ["ab", "cd", "e"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | A version of 'chunksOf' where the parameter can be of any 'Integral' type.
genericChunksOf :: (Integral n) => n -> [a] -> [[a]]
genericChunksOf = chunksOf . fromIntegral

newtype Fst a b = Fst {getFst :: (b, a)}
instance Eq b => Eq (Fst a b) where (==) = (==) `using` (fst . getFst)
instance Ord b => Ord (Fst a b) where compare = compare `using` (fst . getFst)

mkFst :: (a -> b) -> a -> Fst a b
mkFst f x = Fst (f x, x)
