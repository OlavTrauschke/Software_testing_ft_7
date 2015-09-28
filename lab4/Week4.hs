{-# LANGUAGE FlexibleInstances #-}

module Lab4 where

import Control.Monad
import Data.List
import Lecture2
import SetOrd
import System.Random
import Test.QuickCheck

{- Assignment 1
    Time spent:     ~15 minutes each
    No real questions emerged unfortunately, exersizes were done before.
-}

{- Assignment 2
    Time spent:     ~2 hours
-}
-- Manual generator
randomIntSet :: IO (Set Int)
randomIntSet = liftM list2set genIntList -- genIntList function from lecture 2

-- Quickcheck generator, show samples: "sample (arbitrary :: Gen (Set Int))"
instance Arbitrary (Set Int) where
    arbitrary = sized (\n -> liftM list2set (arbitrary :: Gen [Int]))

{- Assignment 3
    Time spent:     ~2 hours
    Olav had to write the prelude himself for the master's entry, 
    so we've used those :)

    Generator functions:
        testSets [n] [prop_*]
        quickcheck [prop_*]
    
    where prop = prop_union, prop_intersect, prop_difference, prop_unionNrElements, 
                 prop_intersectNrElements, prop_differenceNrElements

    Both the quicktest and self written functions pass.
-}
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

-- Helper
setLength :: Set a -> Int
setLength (Set xs) = length xs

-- Test properties
prop_union :: Set Int -> Set Int -> Bool
prop_union (Set s) (Set t) = (Set (sort $ union s t)) == unionSet (Set s) (Set t)

prop_intersect :: Set Int -> Set Int -> Bool
prop_intersect (Set s) (Set t) = (Set (sort $ intersect s t)) == intersectSet (Set s) (Set t)

prop_difference :: Set Int -> Set Int -> Bool
prop_difference (Set s) (Set t) = (Set (s\\t)) == differenceSet (Set s) (Set t)

prop_unionNrElements :: Set Int -> Set Int -> Bool
prop_unionNrElements (Set s) (Set t) = (length $ union s t) == (setLength $ unionSet (Set s) (Set t))

prop_intersectNrElements :: Set Int -> Set Int -> Bool
prop_intersectNrElements (Set s) (Set t) = (length $ intersect s t) == (setLength $ intersectSet (Set s) (Set t))

prop_differenceNrElements :: Set Int -> Set Int -> Bool
prop_differenceNrElements (Set s) (Set t) = (length $ s\\t) == (setLength $ differenceSet (Set s) (Set t))

-- Custom generator
testSets :: Int -> (Set Int -> Set Int -> Bool) -> IO Bool
testSets 0 _ = return True
testSets n p = do
  set1 <- randomIntSet
  set2 <- randomIntSet
  liftM2 (&&) (testSets (n-1) p)
    (liftM2 p (return set1) (return set2))

{- Assignment 4
    Time spent:     ~1 hour, Olav less because of entry
    Questions:
        I have one questions about this chapter and that is why a pre_order is called an order and how this relates to the other types of orders presented in te book    
-}

{- Assignment 5
    Time spent:     ~10 mins
-}
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a,b):xs) = sort $ nub $ (a,b):(b,a):(symClos xs)

{- Assignment 6
    Time spent:     30m
-}
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

trClos :: Ord a => Rel a -> Rel a
trClos x = fp (\y -> sort (union (y @@ y) y)) x


{- Assignment 7
    Time spent:     ~2 hours

    Test usage:
        testRelationsManually [n] [prop_*]
        quicktest [prop_*]
    
    where prop = prop_symClosSymmetric, prop_symClosComplete, prop_trClosTransitive,
                 prop_trClosComplete, prop_isTransitive 
-}

-- Helpers
randomInt :: Int -> Int -> IO Int
randomInt min max = getStdRandom (randomR (min,max))

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

-- Custom testing
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
prop_trClosTransitive rel = prop_isTransitive (trClos rel)

prop_trClosComplete :: (Rel Int) -> Bool
prop_trClosComplete rel = all (\x -> elem x y) rel
  where y = trClos rel

prop_isTransitive :: Rel Int -> Bool
prop_isTransitive r = elem ((sort.nub) [(a,d) | (a,b) <- r, (c,d) <- r, b == c]) (subsequences r)

{- Assignment 8
    Time spent:     ~30m -}
prop_TrSym_vs_SymTr :: Rel Int -> Bool
prop_TrSym_vs_SymTr r = (symClos.trClos) r == (trClos.symClos) r

{-
 > *Assignment8> quickCheck prop_TrSym_vs_SymTr 
 > *** Failed! Falsifiable (after 3 tests):  
 > [(1,2),(3,3)]
 
 Which makes sense. The symmetric closure of [(1,2),(3,3)] = [(1,2),(2,1),(3,3)], and the transitive closure of [(1,2),(2,1),(3,3)] = [(1,1),(1,2),(2,1),(3,3)]

 While the transitive closure of [(1,2),(3,3)] = [(1,2),(3,3)], and the symmetric clusure of [(1,2),(3,3)] = [(1,2),(2,1),(3,3)]

 Thus (symClos.trClos) ≠ (trClos.symClos)
 
 -}

