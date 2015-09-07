module Lab2 where
import Data.List (delete,permutations)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Olav
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
      in (not (noTriangle x y z)) && a^2+b^2 /= c^2

isosceles :: (Num a, Ord a) => a -> a -> a -> Bool
isosceles x y z
  | noTriangle x y z = False
  | otherwise =
      let
        l = [x,y,z]
        a = minimum l
        b = minimum (delete a l)
      in a == b

other :: (Num a, Ord a) => a -> a -> a -> Bool
other x y z
  | noTriangle x y z = False
  | otherwise = not (rectangular x y z|| isosceles x y z)

triangleOlav :: Integer -> Integer -> Integer -> Shape
triangleOlav x y z
  | noTriangle x y z = NoTriangle
  | equilateral x y z = Equilateral
  | rectangular x y z = Rectangular
  | isosceles x y z = Isosceles
  | otherwise = Other

-- Areg
triangleAreg :: Integer -> Integer -> Integer -> Shape
triangleAreg a b c  |   (length [ (x,y,z) | [x,y,z] <- (permutations [a,b,c]), (x+y<=z || a<=0 || b<=0 || c<=0  )]) > 0 =   NoTriangle
        |   a==b && b==c                                            =   Equilateral
        |   (length [ (x,y,z) | [x,y,z] <- (permutations [a,b,c]), x^2+y^2==z^2]) > 0           =   Rectangular
        |   (length [ (x,y,z) | [x,y,z] <- (permutations [a,b,c]), x==y]) > 0               =   Isosceles
        |   otherwise                                           =   Other

-- Jordy
triangleJordy :: Integer -> Integer -> Integer -> Shape
triangleJordy a b c | any (\(a:b:c:_) -> a + b <= c || a <= 0) (permutations [a,b,c]) = NoTriangle
               | a == b && b == c = Equilateral
               | a == b || b == c || a == c = Isosceles
               | any (\(a:b:c:_) -> a^2 + b^2 == c^2) (permutations [a,b,c]) = Rectangular
               | otherwise = Other


-- Test triangle functions
type Sides = (Integer, Integer, Integer)
type IdentFunc = Integer -> Integer -> Integer -> Shape

cases :: [(Sides, Shape)]
cases = [
        ((0, 0, 0), NoTriangle),
        ((1, 1, 1), Equilateral),
        ((3, 4, 5), Rectangular),
        ((1, 2, 2), Isosceles),
        ((5, 123, 5), NoTriangle)
    ]

testFunc :: [(Sides, Shape)] -> IdentFunc -> Int
testFunc cases f =
    let
        n = length cases

        tuplist :: Sides -> [Integer]
        tuplist (a,b,c) = [a,b,c]

        listtup :: [Integer] -> Sides
        listtup [a,b,c] = (a,b,c)

        testCase input expected = 
            all (==True) [
                        f a b c == expected |
                            tests <- permutations (tuplist input),
                            (a, b, c) <- [listtup tests]
                    ]
    in
    length filter testCase cases 
    --print ((show (tuplist (1, 2, 3))) ++ (show (listtup [1, 2, 3])))