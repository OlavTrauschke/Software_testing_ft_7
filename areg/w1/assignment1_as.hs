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

-- Exercise 3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x1:x2:xs) = (x1 : (x2*2) : doubleEveryOther xs)
doubleEveryOther (x1:xs) = (x1: doubleEveryOther xs)

-- Exercise 4
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (\x -> (mod x 10) + (quot x 10)) xs )

-- Exercise 5
luhn :: Integer -> Bool
luhn x = (mod (sumDigits (doubleEveryOther (toRevDigits x))) 10) == 0
