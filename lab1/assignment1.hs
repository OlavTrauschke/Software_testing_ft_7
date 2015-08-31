-- Exercise 1
lastDigit :: Integer -> Integer
lastDigit n = rem n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (div n 10)

-- Exercise 2
toRevDigits :: Integer -> [Integer]
toRevDigits n | n > 0     = lastDigit n : toRevDigits (dropLastDigit n)
              | otherwise = []

-- Exercise 3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:zs) = x:2*y:doubleEveryOther zs
doubleEveryOther x = x