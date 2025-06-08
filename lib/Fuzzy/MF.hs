module Fuzzy.MF where

import           Fuzzy.Base

newtype MF a = MF (a -> Double)

-- | Алгебраїчна
newtype AlgebraMF a = AlgebraMF (a -> Double)

-- | Мінімаксна
newtype MaxMinMF a = MaxMinMF (a -> Double)

-- | Обмежена
newtype BoundedMF m a = BoundedMF (a -> m)

instance Num (MF a) where
  (MF f) + (MF g) = MF (\x -> f x + g x)
  (MF f) * (MF g) = MF (\x -> f x * g x)
  (MF f) - (MF g) = MF (\x -> f x - g x)
  abs (MF f)      = MF (\x -> abs (f x))
  signum (MF f)   = MF (\x -> signum (f x))
  fromInteger n   = MF (\x -> fromInteger n)

instance FuzzySetOps (AlgebraMF a) where
  (AlgebraMF f1) ?|| (AlgebraMF f2) = AlgebraMF (\e -> f1 e + f2 e - f1 e * f2 e)
  (AlgebraMF f1) ?&& (AlgebraMF f2) = AlgebraMF (\e -> f1 e * f2 e)
  fnot (AlgebraMF f) = AlgebraMF (\e -> 1 - f e)

instance FuzzySetOps (MaxMinMF a) where
  (MaxMinMF f1) ?|| (MaxMinMF f2) = MaxMinMF (\e -> max (f1 e) (f2 e))
  (MaxMinMF f1) ?&& (MaxMinMF f2) = MaxMinMF (\e -> min (f1 e) (f2 e))
  fnot (MaxMinMF f) = MaxMinMF (\e -> 1 - f e)

instance (Fractional m, Ord m) => FuzzySetOps (BoundedMF m a) where
  (BoundedMF f1) ?|| (BoundedMF f2) = BoundedMF (\e -> min 1 (f1 e + f2 e))
  (BoundedMF f1) ?&& (BoundedMF f2) = BoundedMF (\e -> max 0 (f1 e + f2 e - 1))
  fnot (BoundedMF f) = BoundedMF (\e -> 1 - f e)
