module Assignment7

where

import Rel
import Data.List
import Assignment6
import Test.QuickCheck

isTransitive :: Rel Int -> Bool
isTransitive r = elem ((sort.nub) [(a,d) | (a,b) <- r, (c,d) <- r, b == c]) (subsequences r)

