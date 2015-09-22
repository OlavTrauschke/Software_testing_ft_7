module Assignment4

where

import Rel
import Data.List

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a,b):xs) = sort $ nub $ (a,b):(b,a):(symClos xs)