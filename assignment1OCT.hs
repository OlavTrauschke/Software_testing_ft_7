import Data.Char

lastDigit :: Integer -> Integer
lastDigit = toInteger.digitToInt.last.show