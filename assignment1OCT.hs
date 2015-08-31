import Data.Char

lastDigit :: Integer -> Integer
lastDigit = toInteger.digitToInt.last.show

dropLastDigit :: Integer -> Integer
dropLastDigit = read.allButLast.show

allButLast :: [a] -> [a]
allButLast (x:y:zs) = x:allButLast (y:zs)
allButLast _ = []