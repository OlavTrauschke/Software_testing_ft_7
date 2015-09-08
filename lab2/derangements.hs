module Derangement where

import Data.List
import System.Random

{-
Approximatly two hours were spent on implementing this function and the test functions below
Executed test:
testRepeated 1 6
"pass on: [0,1,2,3]"
"pass on: [0,1,2,3,4,5]"
"pass on: [0,1,2,3,4]"
"pass on: [0,1]"
"pass on: [0]"
"pass on: [0,1]"
-}

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem ys (permutations xs)

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangement' xs ys
    where
        isDerangement' (x:xs) (y:ys) = x /= y && isDerangement' xs ys
        isDerangement' [] [] = True
        isDerangement' _  _  = False

deran :: [Integer] -> [[Integer]]
deran xs = [x | x <- (permutations xs), isDerangement x xs]

--Tests below, usage: testRepeated 1 [number of tests]. The largest test that might be performed is for input value [number of tests].
--It is advised not to run many tests, because running deran for larger numbers can take a while.
testRepeated :: Int -> Int -> IO ()
testRepeated m n
  | m <= n = do
    x <- fmap toInteger (getRandomInt 1 n)
    xs <- return [0..x-1]
    if (test xs)
      then do print ("pass on: " ++ show xs)
              testRepeated (m+1) n
    else do error ("failed test on: " ++ show x)
  | otherwise = return ()

test :: [Integer] -> Bool
test xs = let y = deran xs
         in testIndependentOfInputElements y && testDependentOfInput xs y

--Method to generate random integers from Lecture 2, found at http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
getRandomInt :: Int -> Int -> IO Int
getRandomInt m n = getStdRandom (randomR (m,n))

--Criteria independent of input
listsOfSameLength :: [[a]] -> Bool
listsOfSameLength (x:y:zs) = (length x) == (length y) && listsOfSameLength (y:zs)
listsOfSameLength _ = True

noNumbersAtOwnPlace :: [Integer] -> Integer -> Bool
noNumbersAtOwnPlace [] _ = True
noNumbersAtOwnPlace (x:xs) y = not (x == y) && noNumbersAtOwnPlace xs (y+1)

successiveNumbers :: [Integer] -> Integer -> Bool
successiveNumbers [] _ = True
successiveNumbers x start = elem start x && successiveNumbers (delete start x) (start+1)

testIndependentOfInput :: [[Integer]] -> Bool
testIndependentOfInput [] = True
testIndependentOfInput x = listsOfSameLength x && testIndependentOfInputElements x

testIndependentOfInputElements :: [[Integer]] -> Bool
testIndependentOfInputElements [] = True
testIndependentOfInputElements (x:xs) = noNumbersAtOwnPlace x 0 && successiveNumbers x 0 && testIndependentOfInputElements xs
                                        && (x:xs) == nub (x:xs)

--Criteria dependent of input
testDependentOfInput :: [Integer] -> [[Integer]] -> Bool
testDependentOfInput xs ys = all (\z -> length z == length xs) ys && length ys == numberOfDerangements (length xs)

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