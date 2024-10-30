module Variant where
import qualified Data.Map              as Map
import           Fuzzy.FiniteUniversum

data ArithOp = Add | Sub | Mul | Div deriving (Eq, Ord, Enum)

instance Show ArithOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data OpsImplementation = Algebraic | MaxMin | Bounded deriving (Show)

calcV :: Int -> Int -> Int
calcV g k = g + k + 1

makeA :: Int -> FuzzyFiniteUniversum Double Double
makeA v = FuzzyFiniteUniversum $ Map.fromAscList [(fromIntegral i, 1 / fromIntegral v) | i <- [0..v]]

makeB :: Int -> FuzzyFiniteUniversum Double Double
makeB v = FuzzyFiniteUniversum $ Map.fromAscList [(0.5 * fromIntegral i, fromIntegral i / (4 * fromIntegral v)) | i <- [0..v]]
