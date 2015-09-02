import Prelude

sumPrimesLTTwoMillion :: Integer
sumPrimesLTTwoMillion = sum (takeWhile (<2000000) (sieve [2..]))

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x:(sieve [y|y <- xs, (mod y x) /= 0])

--C stack overflow