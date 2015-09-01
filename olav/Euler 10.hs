sumPrimesLTTwoMillion :: Integer
sumPrimesLTTwoMillion = sumSieve [2..2000000] 0

sumSieve :: [Integer] -> Integer -> Integer
sumSieve [] result = result
sumSieve (x:xs) result = sumSieve (filter (\y -> rem y x /= 0) xs) (result+x)

--C stack overflow