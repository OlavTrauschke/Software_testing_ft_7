import Data.List (permutations)
-- Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem x (permutations y)

-- Conds:
-- input len == test len
-- 