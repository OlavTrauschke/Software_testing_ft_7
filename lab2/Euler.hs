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