module Fuzzy.MF where

import           Fuzzy.Base

-- | Алгебраїчна
newtype AlgebraMF m a = AlgebraMF (a -> m)

-- | Мінімаксна
newtype MaxMinMF m a = MaxMinMF (a -> m)

-- | Обмежена
newtype BoundedMF m a = BoundedMF (a -> m)

instance (Fractional m, Ord m) => FuzzySetOps (AlgebraMF m a) where
  (AlgebraMF f1) ?|| (AlgebraMF f2) = AlgebraMF (\e -> f1 e + f2 e - f1 e * f2 e)
  (AlgebraMF f1) ?&& (AlgebraMF f2) = AlgebraMF (\e -> f1 e * f2 e)
  fnot (AlgebraMF f) = AlgebraMF (\e -> 1 - f e)

instance (Fractional m, Ord m) => FuzzySetOps (MaxMinMF m a) where
  (MaxMinMF f1) ?|| (MaxMinMF f2) = MaxMinMF (\e -> max (f1 e) (f2 e))
  (MaxMinMF f1) ?&& (MaxMinMF f2) = MaxMinMF (\e -> min (f1 e) (f2 e))
  fnot (MaxMinMF f) = MaxMinMF (\e -> 1 - f e)

instance (Fractional m, Ord m) => FuzzySetOps (BoundedMF m a) where
  (BoundedMF f1) ?|| (BoundedMF f2) = BoundedMF (\e -> min 1 (f1 e + f2 e))
  (BoundedMF f1) ?&& (BoundedMF f2) = BoundedMF (\e -> max 0 (f1 e + f2 e - 1))
  fnot (BoundedMF f) = BoundedMF (\e -> 1 - f e)
