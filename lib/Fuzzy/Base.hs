{-# LANGUAGE TypeFamilies #-}

module Fuzzy.Base where

class Fuzzy f e where
    type Crisp f e
    type Returned f e
    supp :: f e -> Crisp f e
    is :: e -> f e -> Returned f e
    height :: f e -> Returned f e
    core :: f e -> Crisp f e
    alphacut :: f e -> Returned f e -> Crisp f e
    mode :: f e -> Crisp f e

class FuzzySetOps a where
    (?&&) :: a -> a -> a
    (?||) :: a -> a -> a
    fnot :: a -> a
