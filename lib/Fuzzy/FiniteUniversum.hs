{-# LANGUAGE TypeFamilies #-}
module Fuzzy.FiniteUniversum where

import           Data.Map            as Map
import qualified Data.Map.Merge.Lazy as Map
import           Data.Maybe          (fromMaybe)
import           Data.Set            as Set
import           Fuzzy.Base

newtype FuzzyFiniteUniversum v k = FuzzyFiniteUniversum (Map k v)

newtype AlgrebraicFU v k = AlgrebraicFU (Map k v)

newtype MaxMinFU v k = MaxMinFU (Map k v)

newtype BoundedFU v k = BoundedFU (Map k v)

instance (Fractional v, Ord k, Ord v) => Fuzzy (FuzzyFiniteUniversum v) k where
    type Crisp k = Set k
    type Returned (FuzzyFiniteUniversum v) k = v
    supp (FuzzyFiniteUniversum fuzzyMap) = Map.keysSet $ Map.filterWithKey (\_el mfValue -> mfValue > 0) fuzzyMap
    is x (FuzzyFiniteUniversum fuzzyMap) = fromMaybe 0 $ Map.lookup x fuzzyMap
    height (FuzzyFiniteUniversum fuzzyMap) = fromMaybe 0 $ Map.foldlWithKey folder Nothing fuzzyMap
        where folder acc el mfValue = case acc of
                Nothing                    -> Just mfValue
                Just prev | mfValue > prev -> Just mfValue
                _                          -> acc
    core (FuzzyFiniteUniversum fuzzyMap) = Map.keysSet $ Map.filterWithKey (\_el mfValue -> mfValue == 1) fuzzyMap
    alphacut (FuzzyFiniteUniversum fuzzyMap) alpha = Map.keysSet $ Map.filterWithKey (\_el mfValue -> mfValue >= alpha) fuzzyMap
    mode finiteFuzzy@(FuzzyFiniteUniversum fuzzyMap) = Map.keysSet $ Map.filter (== fuzzyMapHeight) fuzzyMap
        where fuzzyMapHeight  = height finiteFuzzy


instance (Num v, Ord k, Ord v) => FuzzySetOps (MaxMinFU v k) where
    (MaxMinFU fuzzyMap1) ?&& (MaxMinFU fuzzyMap2) = MaxMinFU resultIntersection
        where resultIntersection = Map.intersectionWith min fuzzyMap1 fuzzyMap2

    (MaxMinFU fuzzyMap1) ?|| (MaxMinFU fuzzyMap2) = MaxMinFU resultUnion
        where resultUnion = Map.unionWith max fuzzyMap1 fuzzyMap2

    fnot (MaxMinFU fuzzyMap) = MaxMinFU $ Map.map (1 -) fuzzyMap

instance (Num v, Ord k) => FuzzySetOps (AlgrebraicFU v k) where
    (AlgrebraicFU fuzzyMap1) ?|| (AlgrebraicFU fuzzyMap2) = AlgrebraicFU resultUnion
        where
            resultUnion = Map.merge keep keep combine  fuzzyMap1 fuzzyMap2
            keep = Map.mapMissing (\_el mfV -> mfV)
            combine = Map.zipWithMatched (\el mfA mfB -> mfA + mfB - mfA * mfB)

    (AlgrebraicFU fuzzyMap1) ?&& (AlgrebraicFU fuzzyMap2) = AlgrebraicFU resultIntersection
        where resultIntersection = Map.intersectionWith (*) fuzzyMap1 fuzzyMap2
    fnot (AlgrebraicFU fuzzyMap) = AlgrebraicFU $ Map.map (1 -) fuzzyMap


instance (Num v, Ord k, Ord v) => FuzzySetOps (BoundedFU v k) where
    (BoundedFU fuzzyMap1) ?&& (BoundedFU fuzzyMap2) = BoundedFU resultIntersection
        where resultIntersection = Map.intersectionWith (\a b -> max 0 (a + b - 1)) fuzzyMap1 fuzzyMap2

    (BoundedFU fuzzyMap1) ?|| (BoundedFU fuzzyMap2) = BoundedFU resultUnion
        where
            resultUnion = Map.merge minOne minOne combine  fuzzyMap1 fuzzyMap2
            minOne = Map.mapMissing (\_el mfV -> min 1 mfV)
            combine = Map.zipWithMatched (\el mfA mfB -> min 1 (mfA + mfB))

    fnot (BoundedFU fuzzyMap) = BoundedFU $ Map.map (1 -) fuzzyMap

