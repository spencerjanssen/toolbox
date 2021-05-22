# toolbox

### Handy-dandy Haskell tools.

This package contains a small selection of useful functions over the `base` library, as well as convenient alternative class implementations for map types from the `containers` library. It is intended as a lightweight accompaniment to your personal projects, since its small dependency graph likely overlaps with it already.

## Changes in v0.1.1.0
New functions:
- `Data.Tuple.Toolbox`
  - `firstF :: (Functor f) => (a -> f b) -> (a, x) -> f (b, x)`
  - `secondF :: (Functor f) => (a -> f b) -> (x, a) -> f (x, b)`
  - friends of the above for higher tuples
  - `toFirst :: (Functor f) => (a -> f b) -> a -> f (b, a)`
  - `toSecond :: (Functor f) => (a -> f b) -> a -> f (a, b)`
  - `unpairFst :: ((a, b), c) -> (a, b, c)`
  - `unpairSnd :: (a, (b, c)) -> (a, b, c)`
  - `pairFst :: (a, b, c) -> ((a, b), c)`
  - `pairSnd :: (a, b, c) -> (a, (b, c))`
  - `dup :: a -> (a, a)`
- `Data.List.Toolbox.unsnoc :: [a] -> Maybe ([a], a)`
- `Data.List.NonEmpty.Toolbox`
  - `withNonEmpty :: [a] -> (NonEmpty a -> b) -> Maybe b`
  - `whenNonEmpty :: (Monad m) => [a] -> (NonEmpty a -> m ()) -> m ()`
  - `whenNonEmptyM :: (Monad m) => m [a] -> (NonEmpty a -> m ()) -> m ()`
Bug fixes:
- `Data.Time.Timeframe`
  - `intersect` now properly calculates intersections and supports instantaneous overlaps
  - `unions` is now terminating
