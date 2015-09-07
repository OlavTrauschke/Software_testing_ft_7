module Triangles where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | any (\(a:b:c:_) -> a + b <= c || a <= 0) (permutations [a,b,c]) = NoTriangle
               | a == b && b == c = Equilateral
               | a == b || b == c || a == c = Isosceles
               | any (\(a:b:c:_) -> a^2 + b^2 == c^2) (permutations [a,b,c]) = Rectangular
               | otherwise = Other

--testR :: Int -> Int -> (Integer -> Integer -> Integer -> Shape)
--                    -> (Integer -> Integer -> Integer -> Shape -> Bool) -> IO ()
--testR k n f r = if k == n then print (show n ++ " tests passed")
--                else do
--                  xs <- genIntTripplesList
--                  if r xs (f xs) then
--                    do print ("pass on: " ++ show xs)
--                       testR (k+1) n f r
--                  else error ("failed test on: " ++ show xs)

--testRel :: (Integer -> Integer -> Integer -> Shape) -> (Integer -> Integer -> Integer -> Shape -> Bool) -> IO ()
--testRel f r = testR 1 100 f r 