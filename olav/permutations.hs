module Lab2 where

import Control.Monad.IO.Class
import Data.List
import System.Random

--Approximatly four hours were spent on implementing this function and the test functions below

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y = elem x (permutations y)

--Tests below, usage: testRepeated 1 [number of tests] isPermutation
--This tests isPermutation for two random lists and for the first random list and a random permutation of that list
testRepeated :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testRepeated m n f
  | m <= n = do
    xs <- fmap nub genIntList
    ys <- fmap nub genIntList
    if (test f xs ys)
      then do print ("pass on: " ++ show xs ++ " " ++ show ys)
      else do error ("failed test on: " ++ show xs ++ " " ++ show ys)
    index <- getRandomInt ((fac (length xs))-1)
    xsPerms <- fmap permutations (return xs)
    xsPerm <- fmap (!! index) (return xsPerms)
    if (m+1 <= n)
      then do if (test f xs xsPerm)
                then do print ("pass on: " ++ show xs ++ " " ++ show xsPerm)
                        testRepeated (m+2) n f
                else do error ("failed test on: " ++ show xs ++ " " ++ show xsPerm)
      else do return ()
  | otherwise = return ()

test :: ([Int] -> [Int] -> Bool) -> [Int] -> [Int] -> Bool
test f xs ys = (f xs ys == (sameLength xs ys && sameElements xs ys)) && (flipEqual f xs ys)

sameLength :: [Int] -> [Int] -> Bool
sameLength x y = (length x) == (length y)

--Since lists may not contain duplicates, lists containing the same elements are permutations of eachother
sameElements :: [Int] -> [Int] -> Bool
sameElements [] [] = True
sameElements _ [] = False
sameElements [] _ = False
sameElements (x:xs) y = elem x y && sameElements xs (y \\ [x])

flipEqual :: ([Int] -> [Int] -> Bool) -> [Int] -> [Int] -> Bool
flipEqual f x y = f x y == f y x

--Example from the lecture for generating random lists of integers, found at http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
--Adapted to higher the changes on finding a permutation
genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n= do
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

fac :: Int -> Int
fac n = fac' n 1

fac' :: Int -> Int -> Int
fac' 0 res = res
fac' n res = fac' (n-1) (n*res)