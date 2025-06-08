module Fuzzy where
import Prelude hiding ((&&), (||), not, and, or, any, all)
import qualified Data.Bool as Bool

class Logic a where
    true, false :: a
    (&&), (||) :: a -> a -> a
    not :: a -> a

and, or :: Logic a => [a] -> a
and = foldr (&&) true
or = foldr (||) false

any, all :: Logic b => (a -> b) -> [a] -> b

any p = or . map p
all p = and . map p

instance Logic Double where
    true = 1
    false = 0
    (&&) = min
    (||) = max
    not x = 1 - x

instance Logic Bool where
    true = True
    false = False
    (&&) = (Bool.&&)
    (||) = (Bool.||)
    not = Bool.not

type Fuzzy a = a -> Double

