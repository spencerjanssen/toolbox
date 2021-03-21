# Changelog for toolbox

## v0.1.1.0
New functions:
- `Data.Function.Toolbox`
  - `firstF :: (Functor f) => (a -> f b) -> (a, x) -> f (b, x)`
  - `secondF :: (Functor f) => (a -> f b) -> (x, a) -> f (x, b)`
  - friends of the above for higher tuples

## v0.1.0.0
The first release contains a small selection of useful functions over the `base` libraries, as well as convenient alternative class implementations for map types from the `containers` library.

- `Data.Map.Monoidal` and `Data.IntMap.Monoidal`
  - Both strict and lazy versions
  - Interface designed to closely match the `containers` versions
- `Data.Time.Timeframe`
  - Bounded and unbounded time intervals
  - Unions and intersections of timeframes
- `M.Toolbox` utilities
  - Additional functions with intuitive names
