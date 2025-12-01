module IxVec where

import Data.Vector
import Data.Ix





ixer1 :: Ix i => (a,a) -> (Int -> a) -> i -> a
ixer1 f i = f . i