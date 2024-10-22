module Fuzzy.Interval where

data Bound a = Include a | Exclude a

-- | A real interval 
data Interval a = To (Bound a) | Between (Bound a) (Bound a) | From (Bound a) 
    
