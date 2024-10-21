{-# LANGUAGE TypeFamilies #-}

module Fuzzy.Base where

class Fuzzy f e where
    type Crisp e
    type Returned f e
    supp :: f e -> Crisp e
    is :: e -> f e -> Returned f e
    height :: f e -> Returned f e
    core :: f e -> Crisp e
    alphacut :: f e -> Returned f e -> Crisp e
    mode :: f e -> Crisp e

class FuzzySetOps a where
    (?&&) :: a -> a -> a
    (?||) :: a -> a -> a
    fnot :: a -> a
