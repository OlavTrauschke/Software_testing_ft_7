import Prelude

--Executing this results is 142913828922
sumPrimesLTTwoMillion :: Integer
sumPrimesLTTwoMillion = sum (takeWhile (<2000000) primes)

primes :: [Integer]
primes = 2:filter prime [3..]

prime :: Integer -> Bool
prime x = all (\ y -> rem x y /=0) (takeWhile (\ y -> y^2 <= x) primes)