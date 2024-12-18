{-# LANGUAGE TypeFamilies #-}
module Fuzzy.TriangleMF where

import           Fuzzy.Base
import           Fuzzy.Interval

-- | Трикутне число
data TriangleMF a = TriangleMF {triangleA :: a, triangleB :: a, triangleC :: a}
instance (Show a) => Show (TriangleMF a) where
  show (TriangleMF a b c) = "Triangle " ++ show (a,b,c)

instance (Num a, Ord a) => Num (TriangleMF a) where
    (TriangleMF a1 b1 c1) + (TriangleMF a2 b2 c2) = TriangleMF a3 b3 c3 where
        a3 = minimum [a1 + a2, a1 + c2, c1 + a2, c1 + c2]
        b3 = b1 + b2
        c3 = maximum [a1 + a2, a1 + c2, c1 + a2, c1 + c2]

    (TriangleMF a1 b1 c1) - (TriangleMF a2 b2 c2) = TriangleMF a3 b3 c3 where
        a3 = minimum [a1-a2, a1 - c2, c1 - a2, c1 - c2]
        b3 = b1 - b2
        c3 = maximum [a1-a2, a1 - c2, c1 - a2, c1 - c2]

    (TriangleMF a1 b1 c1) * (TriangleMF a2 b2 c2) = TriangleMF a3 b3 c3 where
        a3 = minimum [a1*a2, a1 * c2, c1 * a2, c1 * c2]
        b3 = b1 * b2
        c3 = maximum [a1*a2, a1 * c2, c1 * a2, c1 * c2]
    abs (TriangleMF a b c) = TriangleMF (abs a) (abs b) (abs c)

    signum (TriangleMF a b c) = TriangleMF (signum a) (signum b) (signum c)

    fromInteger i = TriangleMF fromI fromI fromI where fromI = fromInteger i

instance (Fractional a, Ord a) => Fractional (TriangleMF a) where
    (TriangleMF a1 b1 c1) / (TriangleMF a2 b2 c2) = TriangleMF a3 b3 c3 where
        a3 = a1 / a2
        b3 = b1 / b2
        c3 = c1 / c2
    fromRational r = TriangleMF fromR fromR fromR where fromR = fromRational r

instance (Fractional k, Ord k) => Fuzzy TriangleMF k  where
    type Crisp TriangleMF k = [Interval k]
    type Returned TriangleMF k = k
    supp (TriangleMF a b c) = [Between (Exclude a) (Exclude c)]
    is x (TriangleMF a b c) | x <= a || x >= c = 0
                            | a <= x && x <= b = (x - a) / (b - a)
                            | b <= x && x <= c = (c - x) / (c - b)

    height = const 1
    core = mode
    alphacut (TriangleMF a b c) alpha
        | alpha <= 0 = [To (Include a), From (Include c)]
        | otherwise =
            [From (Include (alpha * b + a * (1 - alpha))),
             To (Include (c - alpha * (c - b)))]


    mode (TriangleMF _a b _c) = [Between (Include b) (Include b)]
