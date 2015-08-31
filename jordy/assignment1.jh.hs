import HW01Tests

doubleOdd :: [Integer] -> [Integer]
doubleOdd [] = []
doubleOdd [x] = [(toInteger (2*x))]
doubleOdd (x:y:zs) = (toInteger(2*x)) : y : doubleOdd zs

lastDigit :: Integer -> Integer
lastDigit n = rem n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (div n 10)

toRevDigits :: Integer -> [Integer]
toRevDigits n | n > 0     = lastDigit n : toRevDigits (dropLastDigit n)
              | otherwise = []