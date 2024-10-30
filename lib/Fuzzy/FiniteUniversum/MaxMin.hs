module Fuzzy.FiniteUniversum.MaxMin where
import           Data.Map   as Map
import           Fuzzy.Base

newtype MaxMinFU v k = MaxMinFU (Map k v) deriving Show

instance (Num v, Ord k, Ord v) => FuzzySetOps (MaxMinFU v k) where
    (MaxMinFU fuzzyMap1) ?&& (MaxMinFU fuzzyMap2) = MaxMinFU resultIntersection
        where resultIntersection = Map.intersectionWith min fuzzyMap1 fuzzyMap2

    (MaxMinFU fuzzyMap1) ?|| (MaxMinFU fuzzyMap2) = MaxMinFU resultUnion
        where resultUnion = Map.unionWith max fuzzyMap1 fuzzyMap2

    fnot (MaxMinFU fuzzyMap) = MaxMinFU $ Map.map (1 -) fuzzyMap
