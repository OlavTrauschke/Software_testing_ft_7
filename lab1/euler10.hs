problem10 = sum (takeWhile (<= 2000000) primes)

-- Prime functions below are from the first lecture.

primes :: [Integer]
primes = 2 : filter prime [3..] 

prime :: Integer -> Bool
prime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes