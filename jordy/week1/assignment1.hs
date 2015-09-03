module Assignment1 where

lastDigit :: Integer -> Integer
lastDigit n = rem n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (div n 10)

toRevDigits :: Integer -> [Integer]
toRevDigits n | n > 0     = lastDigit n : toRevDigits (dropLastDigit n)
              | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : (toInteger(2*y)) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (n:ns) | n < 10    = n + (sumDigits ns)
                 | otherwise = (rem n 10) + (div n 10) + (sumDigits ns)


luhn :: Integer -> Bool
luhn n = rem ((sumDigits.doubleEveryOther.toRevDigits) n) 10 == 0