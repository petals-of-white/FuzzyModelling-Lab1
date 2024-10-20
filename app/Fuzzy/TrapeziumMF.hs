module TrapeziumMF where

-- | Трапецієподібне число
data TrapeziumMF a = TrapeziumMF {trapeziumA :: a, trapeziumB :: a, trapeziumC :: a, trapeziumD :: a}

instance (Num a, Ord a) => Num (TrapeziumMF a) where
    (TrapeziumMF a1 b1 c1 d1) + (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = a1 + a2
        b3 = b1 + b2
        c3 = c1 + c2
        d3 = d1 + d2

    (TrapeziumMF a1 b1 c1 d1) - (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = a1 - b2 - d2 + c2
        b3 = b1 - b2
        c3 = c1 - c2
        d3 = d1 + b2 - a2 - c2

    (TrapeziumMF a1 b1 c1 d1) * (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = b1 * a2 - b1 * b2 + a1 * b2
        b3 = b1 * b2
        c3 = c1 * c2
        d3 = c1 * d2 - c1 * c2  + d1 * c2

    abs (TrapeziumMF a b c d) = TrapeziumMF (abs a) (abs b) (abs c) (abs d)

    signum (TrapeziumMF a b c d) = TrapeziumMF (signum a) (signum b) (signum c) (signum d)

    fromInteger i = TrapeziumMF fromI fromI fromI fromI
        where fromI = fromInteger i


instance (Fractional a, Ord a) => Fractional (TrapeziumMF a) where
    (TrapeziumMF a1 b1 c1 d1) / (TrapeziumMF a2 b2 c2 d2) = TrapeziumMF a3 b3 c3 d3 where
        a3 = (b1 * c2 - b1 * d2 + a1 * c2) / (c2 ^ (2 :: Integer))
        b3 = b1/c2
        c3 = c1/b2
        d3 = (c1 * b2 - c1 * a2 + d1 * b2) / (b2 ^ (2 :: Integer))

    fromRational r = TrapeziumMF fromR fromR fromR fromR
        where fromR = fromRational r
