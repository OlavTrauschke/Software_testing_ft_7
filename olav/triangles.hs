module Lab2 where

import Data.List
import System.Random

noTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
noTriangle x y z = x + y < z || x + z < y || y + z < x || x <= 0 || y <= 0 || z <= 0

equilateral :: (Num a, Ord a) => a -> a -> a -> Bool
equilateral x y z
  | noTriangle x y z = False
  | otherwise = x == y && y == z

rectangular :: (Num a, Ord a) => a -> a -> a -> Bool
rectangular x y z
  | noTriangle x y z = False
  | otherwise =
    let
      l = [x,y,z]
      a = minimum l
      c = maximum l
      b = head (delete a (delete c l))
      in a^2 + b^2 == c^2

test x y z = [x,y,z]

isosceles :: (Num a, Ord a) => a -> a -> a -> Bool
isosceles x y z
  | noTriangle x y z = False
  | otherwise = x == y || x == z || y == z

other :: (Num a, Ord a) => a -> a -> a -> Bool
other x y z
  | noTriangle x y z = False
  | otherwise = not (rectangular x y z|| isosceles x y z)

data Shape = NoTriangle | Equilateral
           | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | noTriangle x y z = NoTriangle
  | equilateral x y z = Equilateral
  | rectangular x y z = Rectangular
  | isosceles x y z = Isosceles
  | otherwise = Other