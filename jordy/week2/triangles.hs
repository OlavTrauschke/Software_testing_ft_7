module Triangles where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a <= 0 || b <= 0 || c <= 0 || a + b < c || b + c < a || a + c < b = NoTriangle
               | a == b && b == c = Equilateral
               | a == b || b == c || a == c = Isosceles
               | any (\(a:b:c:_) -> a^2 + b^2 == c^2) (permutations [a,b,c]) = Rectangular
               | otherwise = Other