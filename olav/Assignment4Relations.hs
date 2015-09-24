{-# LANGUAGE FlexibleInstances #-}

module Assignment4Relations

where

import Assignment4Sets
import Control.Monad
import Data.List
import SetOrd hiding (unionSet) --we had to implement unionSet ourselves
import System.Random
import Test.QuickCheck

{-I spent about fifteen minutes rereading chapter 4, of which I previously had
to make all the exercises. I have one questions about this chapter and that is
why a pre_order is called an order and how this relates to the other types of
orders presented in te book.-}

--Exercise 5 below. We spent about 10 minutes on this exercise

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):zs)
  | x==y = (x,y):(symClos zs)
  | otherwise = (x,y):(y,x):(symClos (delete (y,x) zs))

--Exercise 6 below. We spent about half an hour on this exercise

--The operator below was defined in the assignment at
--http://homepages.cwi.nl/~jve/courses/15/testing/lab/Lab4.html
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos x = fp (\y -> sort (union (y @@ y) y)) x

--The operator below was defined in the assignment at
--http://homepages.cwi.nl/~jve/courses/15/testing/lab/Lab4.html
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

--Exercise 7 below. We spent about 2 hours on this exercise.

{-We tested running testRelationsManually 100 and quickCheck on all properties below.-}

--Functions for generating random relations of Ints
instance Arbitrary (Rel Int) where
  arbitrary = liftM nub $ listOf $ (liftM2 (,) (choose (1,10)) (choose (1,10))) 

randomIntRel :: IO (Rel Int)
randomIntRel = do
  n <- randomInt 0 100
  result <- randomIntRel' n
  return (nub result)

randomIntRel' :: Int -> IO (Rel Int)
randomIntRel' 0 = return []
randomIntRel' n = do
  pair <- randomIntPair
  furtherValues <- randomIntRel' (n-1)
  return (pair:furtherValues)

randomIntPair :: IO (Int,Int)
randomIntPair = do
  x <- randomInt 0 10
  y <- randomInt 0 10
  return (x,y)

{-Own implementation of testing. Usage: testSetsManually [numberOfCases]
[prop_symClosSymmetric|prop_symClosComplete|prop_trClosTransitive
|prop_trClosComplete]-}
testRelationsManually :: Int -> ((Rel Int) -> Bool) -> IO Bool
testRelationsManually 0 _ = return True
testRelationsManually n p = do
  rel <- randomIntRel
  liftM2 (&&) (testRelationsManually (n-1) p) (return (p rel))

prop_symClosSymmetric :: (Rel Int) -> Bool
prop_symClosSymmetric rel = isSymmetric (symClos rel)

isSymmetric :: Eq a => (Rel a) -> Bool
isSymmetric [] = True
isSymmetric ((x,y):zs)
  | x==y = isSymmetric zs
  | otherwise = elem (y,x) zs && isSymmetric (delete (y,x) zs)

prop_symClosComplete :: (Rel Int) -> Bool
prop_symClosComplete rel = all (\x -> elem x y) rel
  where y = symClos rel

prop_trClosTransitive :: (Rel Int) -> Bool
prop_trClosTransitive rel = isTransitive (trClos rel)

isTransitive :: (Rel Int) -> Bool
isTransitive = isTransitive' []

isTransitive' :: (Rel Int) -> (Rel Int) -> Bool
isTransitive' _ [] = True
isTransitive' done ((x,y):zs) = (all (\(v,w) -> elem (x,w) ((x,y):(union zs done)))
  (filter (\(v,w) -> v==y) (union zs done))) && isTransitive' ((x,y):done) zs

prop_trClosComplete :: (Rel Int) -> Bool
prop_trClosComplete rel = all (\x -> elem x y) rel
  where y = trClos rel