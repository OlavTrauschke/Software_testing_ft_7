import Data.List (permutations)

-- Permutations, could probably use intersect
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem x (permutations y)

-- [1, 2, 3] [2, 1, 3] -> true
-- [1, 2, 3] [3, 2, 1] -> false
isDeranged :: Eq a => [a] -> [a] -> Bool
isDeranged [] [] = False
isDeranged a b =
    let
        isDeranged' [] [] = True
        isDeranged' (a:as) (b:bs) = a /= b && isDeranged' as bs
    in isPermutation a b && isDeranged' a b

-- Generate list of all derangements of list [0..n-1]
deran :: Integer -> [[Integer]]
deran n = 
    let l = [0..n-1]
    in filter (isDeranged l) (permutations l)