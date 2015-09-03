-- Euler problem 49
-- Team: FT_7
-- Date: 03.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

import Data.List 

-- Prime property
-- http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture1.html
prime :: Int -> Bool
prime n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

-- Sieve of Eratosthenes
-- http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture1.html
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

-- Calculate all permutations of a string
-- http://homepages.cwi.nl/~jve/courses/15/testing/workshops/Workshop1.html
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
        insrt x [] = [[x]]
        insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Number of primes in a list of numbers
n_primes :: [Int] -> Int
n_primes xs = length (filter prime xs)

-- Delete duplicates
del_dupl :: [Int] -> [Int]
del_dupl [] = []
del_dupl (x:xs) = x : del_dupl (filter (\n -> n /= x) xs)

-- Calculate all permutations of a number, and only keep 4-digit number primes 
perms_4dig :: Int ->[Int]
perms_4dig x = 
	let
		perms_str = perms (show x)
		perms_int = map (\p -> read p::Int) perms_str
		perms_4dig_prime = filter (\p -> p>=1000 && prime p) perms_int
	in
		-- Remove duplicate numbers form list
		del_dupl perms_4dig_prime

-- Calculate permutations for each number. All numbers already found as permutation are deleted from the rest of the primes 
minimum_perms :: [Int] -> [[Int]]
minimum_perms [] = []
minimum_perms (x:xs) = 
	let
		perms_x = perms_4dig x
	in
		perms_x : minimum_perms (filter (\p -> not (elem p perms_x)) xs)
	
-- Find all arithmeitc triplet (a,b,c) in a list of numbers, where a<b<c and b-a==c-b
arith_triplets :: [Int] -> [(Int, Int, Int)]
arith_triplets xs = [(a,b,c) | a <- xs, b <- xs, c <- xs, a<b, b<c, c-b==b-a] 

-- Find all arithmetic triplets of primes between 1000 and 9999
find_triplets :: [[(Int,Int,Int)]]
find_triplets = 
	let
		-- Cast all 4-digit primes to Int and calculate sets of primes without duplications
		primes_4dig = map (fromIntegral) (filter (>= 1000) (sieve [2..9999]))
		candidate_sets = filter (\ps -> (length ps) >= 3) (minimum_perms primes_4dig)
		all_triplets = map (arith_triplets) candidate_sets
	in
		(filter (\triplets -> (length triplets) > 0)  all_triplets)  

-- Concatenate the digits of the numbers in a list 
append_ints :: [Int] -> String
append_ints [] = []
append_ints (x:xs) = (show x) ++ (append_ints xs)

-- Euler problem 49
-- https://projecteuler.net/problem=49
problem49 :: Int
problem49 = 
	let
		result_triplet = (filter (\triplets -> (triplets !! 0) /= (1487,4817,8147)) find_triplets) !! 0	
	in
		[ read (append_ints [a,b,c])::Int | (a,b,c) <- result_triplet]  !! 0
	
		

	


















