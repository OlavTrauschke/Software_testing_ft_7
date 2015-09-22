module Assignment3

where

import SetOrd
import Data.List
import Test.QuickCheck
import Assignment2

-- From Assignment 4.54 of "The Haskell Road to Logic, Maths and Programming"
unionSet :: Ord a => Set a -> Set a -> Set a
unionSet (Set [])     (Set ys) = Set ys
unionSet (Set xs)     (Set []) = Set xs
unionSet (Set (x:xs))  ys      = insertSet x (unionSet (Set xs) (deleteSet x ys))

intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (Set [])     (Set ys) = Set []
intersectSet (Set xs)     (Set []) = Set []
intersectSet (Set (x:xs))  ys      | inSet x ys = insertSet x (intersectSet (Set xs) ys)
                                   | otherwise  = intersectSet (Set xs) ys

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet xs      (Set [])     = xs
differenceSet (Set []) _           = Set []
differenceSet xs      (Set (y:ys)) = differenceSet (deleteSet y xs) (Set ys)

setLength :: Set a -> Int
setLength (Set xs) = length xs

prop_notLarger :: (Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool
prop_notLarger f s t = max (setLength s) (setLength t) >= (setLength $ f s t)

prop_notSmaller :: (Set Int -> Set Int -> Set Int) -> Set Int -> Set Int -> Bool
prop_notSmaller f s t = max (setLength s) (setLength t) <= (setLength $ f s t)

prop_unionNrElements :: Set Int -> Set Int -> Bool
prop_unionNrElements (Set s) (Set t) = (length $ union s t) == (setLength $ unionSet (Set s) (Set t))

prop_intersectNrElements :: Set Int -> Set Int -> Bool
prop_intersectNrElements (Set s) (Set t) = (length $ intersect s t) == (setLength $ intersectSet (Set s) (Set t))

prop_differenceNrElements :: Set Int -> Set Int -> Bool
prop_differenceNrElements (Set s) (Set t) = (length $ s\\t) == (setLength $ differenceSet (Set s) (Set t))

prop_union :: Set Int -> Set Int -> Bool
prop_union (Set s) (Set t) = (Set (sort $ union s t)) == unionSet (Set s) (Set t)

prop_intersect :: Set Int -> Set Int -> Bool
prop_intersect (Set s) (Set t) = (Set (sort $ intersect s t)) == intersectSet (Set s) (Set t)

prop_difference :: Set Int -> Set Int -> Bool
prop_difference (Set s) (Set t) = (Set (s\\t)) == differenceSet (Set s) (Set t)