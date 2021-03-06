{-# LANGUAGE FlexibleInstances #-}

module Assignment4Sets

where

import Control.Monad
import Data.List
import SetOrd hiding (unionSet) --we had to implement unionSet ourselves
import System.Random
import Test.QuickCheck

{-I spent about fifteen minutes rereading chapter 4, of which I previously had
to make all the exercises. I have no questions about this chapter.-}

--Exercise 2. We spent about two hours on this exercise.

randomIntSet :: IO (Set Int)
randomIntSet = fmap list2set (genIntList 100 (-100) 100)

--The below random functions are based on Lecture 2 found at
--http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
genIntList :: Int -> Int -> Int -> IO [Int]
genIntList maxElements minValue maxValue = do 
  n <- randomInt 0 maxElements
  getIntL n minValue maxValue

getIntL :: Int -> Int -> Int -> IO [Int]
getIntL 0 _ _ = return []
getIntL n min max = do 
  x <- (randomInt min max)
  xs <- getIntL (n-1) min max
  return (x:xs)

--Get a random Int between two bounds
randomInt :: Int -> Int -> IO Int
randomInt min max = getStdRandom (randomR (min,max))

instance Arbitrary (Set Int) where
  arbitrary = arbSetInt

arbSetInt :: Gen (Set Int)
arbSetInt = liftM list2set (arbitrary :: Gen [Int])

{-Exercise 3 below. I still had my implementations of set intersection, set union and set
difference from when had to make all exercises from The Haskell Road to be allowed to start
the master Software Engineering and 'thus spent only a few minutes to find and check the
code I wrote earlier and pasted below. I spent about two hours implementing tests, and
correcting minor mistakes inmy implementations of set intersection, set union and set
difference.-}

unionSet :: Ord a => Set a -> Set a -> Set a
unionSet (Set []) y = y
unionSet (Set (x:xs)) y = unionSet (Set xs) (insertSet x y)

intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (Set []) y = Set []
intersectSet (Set (x:xs)) y
  | (((length xs) > 0) && (inSet x y)) = insertSet x (intersectSet (Set xs) y)
  | ((length xs) > 0) = intersectSet (Set xs) y
  | (inSet x y) = (Set [x])
  | otherwise = emptySet

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set []) _ = (Set [])
differenceSet (Set (x:xs)) (Set y)
  | inSet x (Set y) = differenceSet (Set xs) (Set y)
  | otherwise = insertSet x (differenceSet (Set xs) (Set y))

{-Own implementation of testing.
Usage: testSetsManually [numberOfCases] [prop_union|prop_intersect|prop_difference]-}

testSetsManually :: Int -> (Set Int -> Set Int -> Bool) -> IO Bool
testSetsManually 0 _ = return True
testSetsManually n p = do
  set1 <- randomIntSet
  set2 <- randomIntSet
  liftM2 (&&) (testSetsManually (n-1) p)
    (liftM2 p (return set1) (return set2))

--Test properties below

--The union of two sets must contain all elements from both sets
prop_union :: Set Int -> Set Int -> Bool
prop_union x y = unionElements x y (unionSet x y)

unionElements :: Ord a => Set a -> Set a -> Set a -> Bool
unionElements (Set []) (Set []) _ = True
unionElements (Set (x:xs)) ys union = (inSet x union) && (unionElements (Set xs) ys union)
unionElements (Set []) (Set (y:ys)) union = (inSet y union)
  && (unionElements (Set []) (Set ys) union)

--The intersection of two sets may hold only elements which are in both sets
prop_intersect :: Set Int -> Set Int -> Bool
prop_intersect x y = intersectionElements x y (intersectSet x y)

intersectionElements :: Ord a => Set a -> Set a -> Set a -> Bool
intersectionElements _ _ (Set []) = True
intersectionElements x y (Set (z:zs)) = inSet z x && inSet z y
  && intersectionElements x y (Set zs)

{-The difference of two sets may hold only elements which are in the first set,
but not in the second-}
prop_difference :: Set Int -> Set Int -> Bool
prop_difference x y = differenceElements x y (differenceSet x y)

differenceElements :: Ord a => Set a -> Set a -> Set a -> Bool
differenceElements _ _ (Set []) = True
differenceElements fst scd (Set (x:xs)) = inSet x fst && not (inSet x scd)
  && differenceElements fst scd (Set xs)