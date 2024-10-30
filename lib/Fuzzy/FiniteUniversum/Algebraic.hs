module Fuzzy.FiniteUniversum.Algebraic where
import           Data.Map            as Map
import qualified Data.Map.Merge.Lazy as Map
import           Fuzzy.Base

newtype AlgrebraicFU v k = AlgrebraicFU (Map k v)

instance (Num v, Ord k) => FuzzySetOps (AlgrebraicFU v k) where
    (AlgrebraicFU fuzzyMap1) ?|| (AlgrebraicFU fuzzyMap2) = AlgrebraicFU resultUnion
        where
            resultUnion = Map.merge keep keep combine  fuzzyMap1 fuzzyMap2
            keep = Map.mapMissing (\_el mfV -> mfV)
            combine = Map.zipWithMatched (\el mfA mfB -> mfA + mfB - mfA * mfB)

    (AlgrebraicFU fuzzyMap1) ?&& (AlgrebraicFU fuzzyMap2) = AlgrebraicFU resultIntersection
        where resultIntersection = Map.intersectionWith (*) fuzzyMap1 fuzzyMap2
    
    fnot (AlgrebraicFU fuzzyMap) = AlgrebraicFU $ Map.map (1 -) fuzzyMap
