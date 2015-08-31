doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:zs) = x:2*y:doubleEveryOther zs
doubleEveryOther x = x