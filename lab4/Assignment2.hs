module Assignment2

where

import SetOrd
import System.Random
import Control.Monad
import Test.QuickCheck

-- From lecture2
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)

{-
 - Custom implementation, using the genIntList function form lecture 2. Also using the list2set function of SetOrd
 -}
generateRandomSet :: IO (Set Int)
generateRandomSet = liftM list2set genIntList


{- 
 - QuickCheck way.
 - To show samples: "sample (arbitrary :: Gen (Set Int))"
 -}
instance Arbitrary (Set Int) where
    arbitrary = sized (\n -> liftM list2set (arbitrary :: Gen [Int]))