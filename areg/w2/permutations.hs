
module Lab2 where
 
import Data.List
import System.Random


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys  	| length (x:xs) /= length ys	= False
				| otherwise			= (elem x ys) && (isPermutation xs (delete x ys))
