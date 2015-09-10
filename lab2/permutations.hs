module Permutations where

import Lecture2
import Data.List
import System.Random
import Control.Monad

-- Sneaky implementation by Jordy
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' xs ys = elem ys (permutations xs)

-- Custom implementation by Areg
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys = (elem x ys) && (isPermutation xs (delete x ys))
isPermutation _ _ = False

-- Altered test function from Lecture 2
testR' :: Int -> Int -> ([Int] -> [Int] -> Bool)
                    -> ([Int] -> [Int] -> Bool -> Bool) -> IO ()
testR' k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList'
                  ys <- randomPermutation (return xs)
                  if r xs ys (f xs ys) then
                    do print ("pass on: " ++ show xs ++ "->" ++ show ys ++ "->" ++ show (f xs ys))
                       testR' (k+1) n f r
                  else error ("failed test on: " ++ show xs)

-- Test if the tested function returns the right anwser
permTest :: Eq a => [a] -> [a] -> Bool -> Bool
permTest xs ys r = elem xs (permutations ys) == r

-- Altered test function from Lecture 2
testRel' :: ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool -> Bool) -> IO ()
testRel' f r = testR' 1 100 f r

-- Altered list generator from Lecture 2
genIntList' :: IO [Int]
genIntList' = do 
  k <- getRandomInt 20
  n <- getRandomInt 5
  getIntL' k n
-- Altered list generator from Lecture 2
getIntL' :: Int -> Int -> IO [Int]
getIntL' _ 0 = return []
getIntL' k n = do 
   x <-  getRandomInt k
   xs <- getIntL' k (n-1)
   return (x:xs)

-- Randomly choose either a permutation of the input list or a new random list
randomPermutation :: IO [Int] -> IO [Int]
randomPermutation xs = do 
   b <- getRandomInt 1
   if b==0 then (fmap (last.permutations) xs) else genIntList'

runtest :: IO ()
runtest = do
  testRel' isPermutation permTest -- Test Areg's function
  testRel' isPermutation' permTest -- Test Jordy's function