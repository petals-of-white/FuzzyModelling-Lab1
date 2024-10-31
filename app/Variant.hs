module Variant where
import qualified Data.Map              as Map
import           Fuzzy.FiniteUniversum
import           Fuzzy.TrapeziumMF
import           Fuzzy.TriangleMF


data ArithOp = Add | Sub | Mul | Div deriving (Eq, Ord, Enum)

data VariantData = VariantData {
  varG :: Int, varK :: Int,
  varV :: Int, varT :: Int,
  varSetA :: FuzzyFiniteUniversum Double Double,
  varSetB :: FuzzyFiniteUniversum Double Double,
  varTriangles :: (TriangleMF Double, TriangleMF Double),
  varTrapezia :: (TrapeziumMF Double, TrapeziumMF Double)
  }

-- | Використовуйте цю функцію для створення даних для варіанту
varData :: Int -> Int -> VariantData
varData g k = VariantData {
  varG = g,
  varK = k,
  varV = v,
  varT = t,
  varSetA = makeA v,
  varSetB = makeB v,
  varTriangles = makeTriangles v t,
  varTrapezia = makeTrapezia v t
}
  where v = calcV g k
        t = calcT v


instance Show ArithOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data OpsImplementation = Algebraic | MaxMin | Bounded deriving (Show)

calcV :: Int -> Int -> Int
calcV g k = g + k + 1

calcT :: Int -> Int
calcT v = v `mod` 3

makeA :: Int -> FuzzyFiniteUniversum Double Double
makeA v = FuzzyFiniteUniversum $ Map.fromAscList [(fromIntegral i, 1 / fromIntegral v) | i <- [0..v]]

makeB :: Int -> FuzzyFiniteUniversum Double Double
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


