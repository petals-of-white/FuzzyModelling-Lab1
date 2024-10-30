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

makeA :: Int -> FuzzyFiniteUniversum Double Int
makeA v = FuzzyFiniteUniversum $ Map.fromAscList [(i, 1 / fromIntegral v) | i <- [0..v]]

makeB :: Int -> FuzzyFiniteUniversum Double Double
makeB v = FuzzyFiniteUniversum $ Map.fromAscList [(0.5 * fromIntegral i, (4 * fromIntegral v) / fromIntegral v) | i <- [0..v]]
