-- Areg Shahbazian, Lab 01

lastDigit :: Integer -> Integer
lastDigit n = read [x]::Integer where (x:xs) = reverse (show n)

dropLastDigit :: Integer -> Integer
dropLastDigit n = read (reverse xs) where (x:xs) = reverse (show n)
