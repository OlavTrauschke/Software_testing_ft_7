
module Permutations where
 
import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys = (elem x ys) && (isPermutation xs (delete x ys))
isPermutation [] _ = False

