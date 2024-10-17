{-# LANGUAGE TypeFamilies #-}
module Fuzzy where

class Fuzzy f e where
    type Crisp e
    supp :: f e -> Crisp e
    is :: e -> f e -> Double
    height :: f e -> Double
    core :: f e -> Crisp e
    alphacut :: f e -> Crisp e
    mode :: f e -> Crisp e

class FuzzyOps a where
    (?&&) :: a -> a -> a
    (?||) :: a -> a -> a
    fnot :: a -> a

newtype AlgebraMF a = AlgebraMF (a -> Double)

instance FuzzyOps (AlgebraMF a) where
  (AlgebraMF f1) ?|| (AlgebraMF f2) = AlgebraMF (\e -> f1 e + f2 e - f1 e * f2 e)
  (AlgebraMF f1) ?&& (AlgebraMF f2) = AlgebraMF (\e -> f1 e * f2 e)
  fnot (AlgebraMF f) = AlgebraMF (\e -> 1 - f e)


newtype MaxMinMF a = MaxMinMF (a -> Double)

instance FuzzyOps (MaxMinMF a) where
  (MaxMinMF f1) ?|| (MaxMinMF f2) = MaxMinMF (\e -> max (f1 e) (f2 e))
  (MaxMinMF f1) ?&& (MaxMinMF f2) = MaxMinMF (\e -> min (f1 e) (f2 e))
  fnot (MaxMinMF f) = MaxMinMF (\e -> 1 - f e)

newtype BoundedMF a = BoundedMF (a -> Double)

instance FuzzyOps (BoundedMF a) where
  (BoundedMF f1) ?|| (BoundedMF f2) = BoundedMF (\e -> min 1 (f1 e + f2 e))
  (BoundedMF f1) ?&& (BoundedMF f2) = BoundedMF (\e -> max 0 (f1 e + f2 e - 1))
  fnot (BoundedMF f) = BoundedMF (\e -> 1 - f e)

data TriangleMF a = TriangleMF {triangleA :: a, triangleB :: a, triangleC :: a}

data TrapeziumMF a = TrapeziumMF {trapeziumA :: a, trapeziumB :: a, trapeziumC :: a, trapeziumD ::a}


instance (Num a, Ord a) => Num (TriangleMF a) where
    (TriangleMF a1 b1 c1) + (TriangleMF a2 b2 c2) = TriangleMF a3 b3 c3 where
        a3 = minimum [a1+a2, a1 + c2, c1 + a2, c1 + c2]
        b3 = b1 + b2
        c3 = maximum [a1+a2, a1 + c2, c1 + a2, c1 + c2]

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
    
    fromInteger i = TriangleMF (fromInteger i) (fromInteger i) (fromInteger i)


instance (Num a, Ord a) => Num (TrapeziumMF a) where
    (TrapeziumMF a1 b1 c1 d1) + (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = minimum [a1+a2, a1 + c2, c1 + a2, c1 + c2]
        b3 = b1 + b2
        c3 = maximum [a1+a2, a1 + c2, c1 + a2, c1 + c2]

    (TrapeziumMF a1 b1 c1 d1) - (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = minimum [a1-a2, a1 - c2, c1 - a2, c1 - c2]
        b3 = b1 - b2
        c3 = maximum [a1-a2, a1 - c2, c1 - a2, c1 - c2]

    (TrapeziumMF a1 b1 c1 d1) * (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = minimum [a1*a2, a1 * c2, c1 * a2, c1 * c2]
        b3 = b1 * b2
        c3 = maximum [a1*a2, a1 * c2, c1 * a2, c1 * c2]
    abs (TrapeziumMF a b c d) = TrapeziumMF (abs a) (abs b) (abs c) (abs d)

    signum (TrapeziumMF a b c d) = TrapeziumMF (signum a) (signum b) (signum c) (signum d)
    
    fromInteger i = TrapeziumMF fromI fromI fromI fromI where fromI = fromInteger i




-- instance (FuzzyOps (MF a)) where
--   (MF f1) ?&& (MF f2) =
--   (?||) = _
--   fnot = _


-- instance (Fractional e, Fuzzy) =>

