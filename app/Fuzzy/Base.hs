{-# LANGUAGE TypeFamilies #-}

module Fuzzy.Base where

class Fuzzy f e where
    type Crisp e
    supp :: f e -> Crisp e
    is :: e -> f e -> Double
    height :: f e -> Double
    core :: f e -> Crisp e
    alphacut :: f e -> Crisp e
    mode :: f e -> Crisp e

class FuzzySetOps a where
    (?&&) :: a -> a -> a
    (?||) :: a -> a -> a
    fnot :: a -> a
