-- Euler problem 50
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

-- To see solution, run:
-- > problem50
-- Finds the solution in 50 seconds (Quad-Core 2.83Ghz, 8GB RAM), 30 seconds of which are used to find the initial list of primes

module Euler50 where

import Data.List

limit :: Integer
limit = 1000000

-- Sieve of Eratosthenes
-- http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture1.html
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

-- Prime property
-- http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture1.html
prime :: Integer -> Bool
prime n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]


-- Return the sum of the first x elements
sumtill xs x
	| x <= (toInteger) (length xs)	= sum $ take (fromInteger x) xs
	| otherwise			= error "x mut be <= |xs|"

-- Find the longest prefix of the list xs that results in a sum that is prime and smaller than the limit. The parameters n and last_prime hold the current prefix length and the length of the last prefix that had a prime number as sum.
longest_prefix :: [Integer] -> Integer -> Integer -> [Integer]
longest_prefix xs n last_prime 
	| newmax >= limit		= take (fromInteger last_prime) xs
	| prime $ newmax		= longest_prefix xs (n+1) n 
	| otherwise			= longest_prefix xs (n+1) last_prime 
	where newmax = sumtill xs n


-- Return the longer of the two lists
longer_list :: [Integer] -> [Integer] -> [Integer]
longer_list xs ys
	| (length xs) > (length ys)	= xs
	| otherwise			= ys


-- Find the longest consecutive prime sequence that results in a sum that is a prime and below the "limit"
longest_seq :: [Integer] -> [Integer] -> [Integer]
longest_seq (x:xs) longest_all  
	-- If no more sequences longer than the longest-of-all sequence are possible any more, return the longest-of-all
	| length (x:xs) < (length longest_all+1)	= longest_all
	-- Otherwise, drop the first element and find the longest possible sequence starting from the second element
	| otherwise					= longest_seq xs longest
	where 	n = (toInteger $ length longest_all ) + 1
		-- Only look for sequences that are longest than the longest-of-all
		longest_now = longest_prefix (x:xs) n 0
		longest = longer_list longest_now longest_all


-- Calculate the sum of the most consecutive primes
problem50 :: Integer
problem50 =
	let
		half = div limit 2
		primes_1 = filter (prime) [2..half]
		primes_2 = take 1 $ filter (prime) [half+1..(limit-1)]
		primes = primes_1 ++ primes_2
	in
		--sum $ longest_seq primes []
		toInteger $ length primes

--Euler problem 56
lastDigit :: Integer -> Integer
lastDigit n = rem n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (div n 10)

toRevDigits :: Integer -> [Integer]
toRevDigits n | n > 0     = lastDigit n : toRevDigits (dropLastDigit n)
              | otherwise = []

sumDigits :: Integer -> Integer
sumDigits x = sum (toRevDigits x)

--Executing problem56 finds 972
problem56 :: Integer
problem56 = maximum [sumDigits (a^b) | a <- [0..100], b <- [0..100]]