import Data.List

problem49 :: Integer
problem49 = (concatIntegers.head) (delete [1487,4817,8147] (filter equalDifferences (createThreeElementSubsetsInList (findPermutationLists fourDigitPrimes))))

--results in 296962999629

concatIntegers :: [Integer] -> Integer
concatIntegers = stringToInteger.concat.(map show)

equalDifferences :: [Integer] -> Bool
equalDifferences = equalElements.differencesList

equalElements :: [Integer] -> Bool
equalElements (x:y:zs)
	| x==y = equalElements (y:zs)
	| otherwise = False
equalElements _ = True

differencesList :: [Integer] -> [Integer]
differencesList (x:y:zs) = (y-x):(differencesList (y:zs))
differencesList _ = []

createThreeElementSubsetsInList :: Eq a => [[a]] -> [[a]]
createThreeElementSubsetsInList [] = []
createThreeElementSubsetsInList (x:xs) = (createSubsetsOfLength x 3) ++ (createThreeElementSubsetsInList xs)

createSubsetsOfLength :: Eq a => [a] -> Int -> [[a]]
createSubsetsOfLength [] _ = [[]]
createSubsetsOfLength _ 0 = [[]]
createSubsetsOfLength (x:xs) n
	| (length (x:xs)) > n = (map (x:) (createSubsetsOfLength xs (n-1))) ++ (createSubsetsOfLength xs n)
	| (length (x:xs)) == n = [(x:xs)]
	| otherwise = [[]]

findPermutationLists :: [Integer] -> [[Integer]]
findPermutationLists [] = []
findPermutationLists (x:xs) = removeListsWithLessThanThreeElements ((intersect (x:xs) (integerPermutations x)):findPermutationLists (xs \\ (integerPermutations x)))

removeListsWithLessThanThreeElements [] = []
removeListsWithLessThanThreeElements (x:xs)
	| length x < 3 = removeListsWithLessThanThreeElements xs
	| otherwise = x:removeListsWithLessThanThreeElements xs

integerPermutations :: Integer -> [Integer]
integerPermutations x = map stringToInteger (permutations (show x))

stringToInteger :: String -> Integer
stringToInteger = read

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = nub [x:y | x <- xs, y <- permutations (xs \\ [x])]

fourDigitPrimes :: [Integer]
fourDigitPrimes = filter prime [1000..9999]

prime :: Integer -> Bool
prime x = all (\ y -> rem x y /=0) (takeWhile (\ y -> y^2 <= x) primes)

primes :: [Integer]
primes = 2:filter prime [3..]