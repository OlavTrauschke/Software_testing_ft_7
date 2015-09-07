module Lab2 where

import Control.Monad.IO.Class
import Data.List
import System.Random

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y = elem x (permutations y)

sameLength :: [Int] -> [Int] -> Bool
sameLength x y = (length x) == (length y)

--Since lists may not contain duplicates,
--lists containing the same elements are permutations of eachother
sameElements :: [Int] -> [Int] -> Bool
sameElements [] [] = True
sameElements _ [] = False
sameElements [] _ = False
sameElements (x:xs) y = elem x y && sameElements xs (y \\ [x])

flipEqual :: ([Int] -> [Int] -> Bool) -> [Int] -> [Int] -> Bool
flipEqual f x y = f x y == f y x

testRepeated :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testRepeated m n f
  | m < n = do
    xs <- fmap nub genIntList
    ys <- fmap nub genIntList
      if test f xs ys
      then do print ("pass on: " ++ show xs ++ " " ++ show ys)
        testRepeated (m+1) n f
      else error ("failed test on: " ++ show xs ++ " " ++ show ys)
  | otherwise = do
    xs <- fmap nub genIntList
    ys <- fmap nub genIntList
    if test f xs ys
      then do print ("pass on: " ++ show xs ++ " " ++ show ys)
        testRepeated (m+1) n f
      else error ("failed test on: " ++ show xs ++ " " ++ show ys)

test :: ([Int] -> [Int] -> Bool) -> [Int] -> [Int] -> Bool
test f xs ys = (f xs ys == (sameLength xs ys && sameElements xs ys)) && (flipEqual f xs ys)

--Example from the lecture for generating random lists of integers, found at http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
--Adapted to fit in the context
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