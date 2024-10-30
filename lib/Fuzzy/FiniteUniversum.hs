{-# LANGUAGE TypeFamilies #-}
module Fuzzy.FiniteUniversum where

import qualified Data.List                       as List
import           Data.Map                        as Map
import qualified Data.Map.Merge.Lazy             as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Set                        as Set
import           Fuzzy.Base
import           Fuzzy.FiniteUniversum.Algebraic
import           Fuzzy.FiniteUniversum.Bounded
import           Fuzzy.FiniteUniversum.MaxMin

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
