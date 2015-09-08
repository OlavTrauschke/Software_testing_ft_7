import Data.List
import System.Random
--Approximatly two hours were spent on implementing this function and the test functions below

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y = elem x (permutations y)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement _ [] = False
isDerangement [] _ = False
isDerangement x y = (isPermutation x y) && noSameElements x y

noSameElements :: Eq a => [a] -> [a] -> Bool
noSameElements [] _ = True
noSameElements _ [] = True
noSameElements (x:xs) (y:ys) = (x /= y) && (noSameElements xs ys)

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) (permutations [0..n-1])

--Tests below, usage: testRepeated 1 [number of tests]. The largest test that might be performed is for input value [number of tests].
--It is advised not to run many tests, because running deran for larger numbers can take a while.
testRepeated :: Int -> Int -> IO ()
testRepeated m n
  | m <= n = do
    x <- getRandomInt 1 n
    if (test x)
      then do print ("pass on: " ++ show x)
              testRepeated (m+1) n
    else do error ("failed test on: " ++ show x)
  | otherwise = return ()

test :: Int -> Bool
test x = let y = deran x
         in testIndependentOfInputElements y && testDependentOfInput x y

--Method to generate random integers from Lecture 2, found at http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
getRandomInt :: Int -> Int -> IO Int
getRandomInt m n = getStdRandom (randomR (m,n))

--Criteria independent of input
listsOfSameLength :: [[a]] -> Bool
listsOfSameLength (x:y:zs) = (length x) == (length y) && listsOfSameLength (y:zs)
listsOfSameLength _ = True

noNumbersAtOwnPlace :: [Int] -> Int -> Bool
noNumbersAtOwnPlace [] _ = True
noNumbersAtOwnPlace (x:xs) y = not (x == y) && noNumbersAtOwnPlace xs (y+1)

successiveNumbers :: [Int] -> Int -> Bool
successiveNumbers [] _ = True
successiveNumbers x start = elem start x && successiveNumbers (delete start x) (start+1)

testIndependentOfInput :: [[Int]] -> Bool
testIndependentOfInput [] = True
testIndependentOfInput x = listsOfSameLength x && testIndependentOfInputElements x

testIndependentOfInputElements :: [[Int]] -> Bool
testIndependentOfInputElements [] = True
testIndependentOfInputElements (x:xs) = noNumbersAtOwnPlace x 0 && successiveNumbers x 0 && testIndependentOfInputElements xs

--Criteria dependent of input
testDependentOfInput :: Int -> [[Int]] -> Bool
testDependentOfInput n x = all (\y -> length y == n) x && length x == numberOfDerangements n

--Implementation of formula found at https://mikespivey.wordpress.com/2011/11/22/derangements/
numberOfDerangements :: Int -> Int
numberOfDerangements n = round (fromIntegral (fac n) * numberOfDerangementsRecursive 0 n 0)

numberOfDerangementsRecursive :: Int -> Int -> Float -> Float
numberOfDerangementsRecursive curr lim res
  | curr <= lim = numberOfDerangementsRecursive (curr+1) lim (res + (-1) ^ curr / fromIntegral (fac curr))
  | otherwise = res

fac :: Int -> Int
fac n = (fac' n 1)

fac' :: Int -> Int -> Int
fac' 0 res = res
fac' n res = fac' (n-1) (n*res)