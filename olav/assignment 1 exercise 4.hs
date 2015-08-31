sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toRevDigits x) + sumDigits xs







toRevDigits :: Integer -> [Integer]
toRevDigits n | n > 0     = lastDigit n : toRevDigits (dropLastDigit n)
              | otherwise = []

-- Exercise 1
lastDigit :: Integer -> Integer
lastDigit n = rem n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (div n 10)