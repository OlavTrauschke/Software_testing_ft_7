module Permutations where

import Data.List
import System.Random

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem ys (permutations xs)