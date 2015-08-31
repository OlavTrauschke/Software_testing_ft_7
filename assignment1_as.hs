-- Areg Shahbazian, Lab 01

-- Exercise 1
lastDigit :: Integer -> Integer
lastDigit n = read [x]::Integer where (x:xs) = reverse (show n)

dropLastDigit :: Integer -> Integer
dropLastDigit n = read (reverse xs) where (x:xs) = reverse (show n)


-- Exercise 2

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n	| n<=0		= []
		| otherwise	= (mod n 10) : toRevDigits (quot n 10)



