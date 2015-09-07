
module Lab2 where
 
import Data.List
import System.Random


data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c	|	(length [ (x,y,z) | [x,y,z] <- (permutations [a,b,c]), (x+y<=z || a<=0 || b<=0 || c<=0  )]) > 0	= 	NoTriangle
		|	a==b && b==c											=	Equilateral
		|	(length [ (x,y,z) | [x,y,z] <- (permutations [a,b,c]), x^2+y^2==z^2]) > 0			=  	Rectangular
		|	(length [ (x,y,z) | [x,y,z] <- (permutations [a,b,c]), x==y]) > 0				=	Isosceles
		|	otherwise											=	Other



