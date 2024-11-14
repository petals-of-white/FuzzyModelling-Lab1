module Variant where
import qualified Data.Map              as Map
import           Fuzzy.Base
import           Fuzzy.FiniteUniversum
import           Fuzzy.TrapeziumMF
import           Fuzzy.TriangleMF


data ArithOp = Add | Sub | Mul | Div deriving (Eq, Ord, Enum)

type SimpleFuzzy = FuzzyFiniteUniversum Double Double

data VariantData = VariantData {
  varG :: Int, varK :: Int,
  varV :: Int, varT :: Int,
  varAlpha :: Double,
  varImplmentation :: OpsImplementation,
  varSetA :: SimpleFuzzy,
  varSetB :: SimpleFuzzy,
  varSetUnion :: SimpleFuzzy,
  varSetIntersection :: SimpleFuzzy,
  varNotA :: SimpleFuzzy,
  varNotB :: SimpleFuzzy,
  varTriangles :: (TriangleMF Double, TriangleMF Double),
  varTrapezia :: (TrapeziumMF Double, TrapeziumMF Double)
  }

-- | Використовуйте цю функцію для створення даних для варіанту
varData :: Int -> Int -> Double -> VariantData
varData g k alpha = VariantData {
  varG = g,
  varK = k,
  varV = v,
  varT = t,
  varAlpha = alpha,
  varImplmentation = implementation,
  varSetA = setA,
  varSetB = setB,
  varSetUnion = setOr,
  varSetIntersection = setAnd,
  varNotA = notA,
  varNotB = notB,
  varTriangles = makeTriangles v t,
  varTrapezia = makeTrapezia v t
}
  where v = calcV g k
        t = calcT v
        setA@(FuzzyFiniteUniversum fuzzyA) = makeA v
        setB@(FuzzyFiniteUniversum fuzzyB) = makeB v

        implementation =
            case t of
                0 -> MaxMin
                1 -> Algebraic
                2 -> Bounded
                _ -> MaxMin

        (setOr, setAnd, notA, notB) = case implementation of
                        MaxMin ->
                            let a = MaxMinFU fuzzyA
                                b = MaxMinFU fuzzyB

                                (MaxMinFU maxMinOr) = a ?|| b
                                (MaxMinFU maxMinAnd) = a ?&& b
                                (MaxMinFU maxMinNotA) = fnot (MaxMinFU fuzzyA)
                                (MaxMinFU maxMinNotB) = fnot (MaxMinFU fuzzyB)
                            in (FuzzyFiniteUniversum maxMinOr, FuzzyFiniteUniversum maxMinAnd,
                                FuzzyFiniteUniversum maxMinNotA, FuzzyFiniteUniversum maxMinNotB)

                        Algebraic ->
                            let a = AlgrebraicFU fuzzyA
                                b = AlgrebraicFU fuzzyB

                                (AlgrebraicFU algOr) = a ?|| b
                                (AlgrebraicFU algAnd) = a ?&& b
                                (AlgrebraicFU algNotA) = fnot (AlgrebraicFU fuzzyA)
                                (AlgrebraicFU algNotB) = fnot (AlgrebraicFU fuzzyB)

                            in (FuzzyFiniteUniversum algOr, FuzzyFiniteUniversum algAnd,
                                FuzzyFiniteUniversum algNotA, FuzzyFiniteUniversum algNotB)
                        Bounded ->
                            let a = BoundedFU fuzzyA
                                b = BoundedFU fuzzyB

                                (BoundedFU boundedOr) = a ?|| b
                                (BoundedFU boundedAnd) = a ?&& b
                                (BoundedFU boundedNotA) = fnot (BoundedFU fuzzyA)
                                (BoundedFU boundedNotB) = fnot (BoundedFU fuzzyB)

                            in (FuzzyFiniteUniversum boundedOr, FuzzyFiniteUniversum boundedAnd,
                                FuzzyFiniteUniversum boundedNotA, FuzzyFiniteUniversum boundedNotB)



instance Show ArithOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data OpsImplementation = Algebraic | MaxMin | Bounded deriving Show

calcV :: Int -> Int -> Int
calcV g k = g + k + 1

calcT :: Int -> Int
calcT v = v `mod` 3

makeA :: Int -> SimpleFuzzy
makeA v = FuzzyFiniteUniversum $ Map.fromAscList [(fromIntegral i, 1 / fromIntegral v) | i <- [0..v]]

makeB :: Int -> SimpleFuzzy
makeB v = FuzzyFiniteUniversum $ Map.fromAscList [(0.5 * fromIntegral i, fromIntegral i / (4 * fromIntegral v)) | i <- [0..v]]

makeTriangles :: Int -> Int -> (TriangleMF Double, TriangleMF Double)
makeTriangles v t =
  ( TriangleMF (realToFrac $ v - t - 1) (realToFrac v) (realToFrac $ v + t + 1),
    TriangleMF (realToFrac $ 2 * v - t) (realToFrac $ 2 * v + 1) (realToFrac $ 2 * v + 5))

makeTrapezia :: Int -> Int -> (TrapeziumMF Double, TrapeziumMF Double)
makeTrapezia v t =
  ( TrapeziumMF (realToFrac (v-t-2)) vFrac (vFrac+0.5) (realToFrac $ v + t + 2),
    TrapeziumMF (realToFrac $ 2 * v - t -1) (realToFrac $ 2 * v) (realToFrac $ 2 * v + 1) (realToFrac $ 2 * v + 3))
  where
    vFrac = realToFrac v


