{-# LANGUAGE FlexibleInstances #-}

module Assignment4

where

import Control.Monad
import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

{-I spent about fifteen minutes rereading chapter 4, of which I previously had
to make all the exercises. I have no questions about this chapter.-}

--Exercise 2. We spent about two hours on this exercise.
randomIntSet :: IO (Set Int)
randomIntSet = fmap list2set genIntList

--The below random functions are based on Lecture 2 found at
--http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
genIntList :: IO [Int]
genIntList = do 
  n <- randomPositiveInt
  getIntL n

getIntL :: Int -> IO [Int]
getIntL 0 = return []
getIntL n = do 
   x <- randomInt
   xs <- getIntL (n-1)
   return (x:xs)

--Get a random Int between -100 and 100
randomInt :: IO Int
randomInt = getStdRandom (randomR (-100,100))

--Get a random Int between 1 and 100
randomPositiveInt :: IO Int
randomPositiveInt = getStdRandom (randomR (1,100))

instance Arbitrary (Set Int) where
	arbitrary = arbSetInt

arbSetInt :: Gen (Set Int)
arbSetInt = liftM list2set (arbitrary :: Gen [Int])