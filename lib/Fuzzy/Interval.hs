module Fuzzy.Interval where

data Bound a = Include a | Exclude a

data Interval a = Interval {rangeLeft :: Bound a, rangeRight :: Bound a}
