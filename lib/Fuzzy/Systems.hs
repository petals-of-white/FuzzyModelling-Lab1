{-# LANGUAGE TypeFamilies #-}
module Fuzzy.Systems where
import           Fuzzy.Base
import           Fuzzy.MF

data Rule a c = Rule {antecedent :: a, consequent :: c, weight :: Double}


minActivation :: (a -> Double) -> Double -> a -> Double
minActivation fs c e = min (fs e) c

centreOfGravity :: [a] -> (a -> Double) -> a
centreOfGravity = undefined

rulebase :: FuzzySetOps a => (a -> a -> a) -> [a] -> a
rulebase = foldr1

(==>) :: a -> (b, Double) -> Rule a b
a ==> (c, w) = Rule a c w

inferrenceSystem :: (FuzzySetOps f, Fractional f) =>
     (f -> f -> f) -> (f -> f -> f) -> (f -> e) -> [Rule f f] -> e

inferrenceSystem activate accum defuzzy rules =
    defuzzy $ rulebase accum $ map (\(Rule a c w) -> activate (a * realToFrac w) c) rules

mamdani :: (FuzzySetOps f, Fractional f) => [Rule f f] -> e
mamdani = inferrenceSystem minActivation (?||) centreOfGravity


-- up :: Double -> Double -> Fuzzy Double
-- up a b x
--     | x < a = 0.0
--     | x < b = (x - a) / (b - a)
--     | otherwise = 1.0

-- -- class (Fuzzy f e) => FRule f e where
-- --     type Antecedent f e
-- --     (==>) :: Antecedent f e -> f e -> f e
-- --     weight :: f e -> Double -> f e

-- grade :: Double
-- grade = 10

-- good :: MF Double Double
-- good = up 10 10

-- testL :: [MaxMinMF Double Double]
-- testL = [good]
