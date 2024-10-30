{-# LANGUAGE TypeFamilies #-}
module Fuzzy.FiniteUniversum where

import qualified Data.List           as List
import           Data.Map            as Map
import qualified Data.Map.Merge.Lazy as Map
import           Data.Maybe          (fromMaybe)
import           Data.Set            as Set
import           Fuzzy.Base

newtype FuzzyFiniteUniversum v k = FuzzyFiniteUniversum { finiteMap :: Map k v}

third :: (a, b, c) -> c
third (_,_, x) = x

fuzzyArithOp :: (Eq k, Ord v, Ord k) => FuzzyFiniteUniversum v k -> FuzzyFiniteUniversum v k -> (k -> k -> k) -> FuzzyFiniteUniversum v k
fuzzyArithOp (FuzzyFiniteUniversum fuzzyA) (FuzzyFiniteUniversum fuzzyB) binOp =
    let     c = [(a, b) | a <- Map.toList fuzzyA, b <- Map.toList fuzzyB]
            groups =
                List.groupBy (\(_, _, opRes1) (_, _, opRes2) -> opRes1 == opRes2) $
                List.sortOn third $ List.map (\((a,mfa),(b,mfb)) -> ((a,mfa), (b,mfb), a `binOp` b)) c

            fuzzyOpResult  = [(third $ head gr, maximum groupMins)
                        | gr <- groups,
                        let groupMins = [min muA muB | ((a,muA), (b, muB), grSum) <- gr]]

        in
            FuzzyFiniteUniversum (Map.fromList fuzzyOpResult)

instance (Num k, Num v, Eq k, Ord k, Ord v) => Num (FuzzyFiniteUniversum v k) where
    fuzzyA + fuzzyB = fuzzyArithOp fuzzyA fuzzyB (+)
    fuzzyA * fuzzyB = fuzzyArithOp fuzzyA fuzzyB (*)
    abs (FuzzyFiniteUniversum fuzzyMap) = FuzzyFiniteUniversum (Map.mapKeys abs fuzzyMap)
    signum (FuzzyFiniteUniversum fuzzyMap) = FuzzyFiniteUniversum (Map.mapKeys signum fuzzyMap)
    fromInteger v = FuzzyFiniteUniversum (Map.singleton (fromInteger v) 1)
    negate (FuzzyFiniteUniversum fuzzyMap) = FuzzyFiniteUniversum (Map.mapKeys negate fuzzyMap)

instance (Fractional k, Num v, Ord k, Ord v) => Fractional (FuzzyFiniteUniversum v k) where
  fromRational rat = FuzzyFiniteUniversum (Map.singleton (fromRational rat) 1)
  fuzzyA / fuzzyB = fuzzyArithOp fuzzyA fuzzyB (/)

newtype AlgrebraicFU v k = AlgrebraicFU (Map k v)

newtype MaxMinFU v k = MaxMinFU (Map k v) deriving Show

newtype BoundedFU v k = BoundedFU (Map k v) deriving Show

instance (Fractional v, Ord k, Ord v) => Fuzzy (FuzzyFiniteUniversum v) k where
    type Crisp (FuzzyFiniteUniversum v) k = Set k
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

