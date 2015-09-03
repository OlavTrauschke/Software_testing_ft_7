problem9 = head [ a * b * c | (a, b, c) <- pythagoreanTriplets, a + b + c == 1000]
    where pythagoreanTriplets = [(a, b, c) | c <- [2..], b <- [1..(c-1)], a <- [0..(b-1)], a^2 + b^2 == c^2]

problem10 = sum (takeWhile (<= 2000000) primes)

problem49 = [x | x <- primes, y <- [x..9999], x > 1000, prime y]

primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes