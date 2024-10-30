module Fuzzy.FiniteUniversum.Bounded where
import           Data.Map            as Map
import qualified Data.Map.Merge.Lazy as Map
import           Fuzzy.Base

newtype BoundedFU v k = BoundedFU (Map k v) deriving Show

instance (Num v, Ord k, Ord v) => FuzzySetOps (BoundedFU v k) where
    (BoundedFU fuzzyMap1) ?&& (BoundedFU fuzzyMap2) = BoundedFU resultIntersection
        where resultIntersection = Map.intersectionWith (\a b -> max 0 (a + b - 1)) fuzzyMap1 fuzzyMap2

    (BoundedFU fuzzyMap1) ?|| (BoundedFU fuzzyMap2) = BoundedFU resultUnion
        where
            resultUnion = Map.merge minOne minOne combine  fuzzyMap1 fuzzyMap2
            minOne = Map.mapMissing (\_el mfV -> min 1 mfV)
            combine = Map.zipWithMatched (\el mfA mfB -> min 1 (mfA + mfB))

    fnot (BoundedFU fuzzyMap) = BoundedFU $ Map.map (1 -) fuzzyMap
