module Derangement where

import Permutations
import Data.List

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangement' xs ys
    where
        isDerangement' (x:xs) (y:ys) = x /= y && isDerangement' xs ys
        isDerangement' [] [] = True
        isDerangement' _  _  = False

deran :: [Integer] -> [[Integer]]
deran xs = [x | x <- (permutations xs), isDerangement x xs]