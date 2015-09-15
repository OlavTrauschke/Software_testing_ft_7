module Assignment3

where


import Data.List
import System.Random
import Lecture3
import Assignment1
import Test.QuickCheck
import Control.Applicative
import Control.Monad


toCnf :: Form -> Form
toCnf f | tautology f = let name = head $ propNames f in (Dsj [(Prop name), (Neg (Prop name))])
        | otherwise   = Cnj (map (\x -> (Dsj (map (\(n,b) -> if b then (Neg (Prop n)) else (Prop n)) x))) (filter (\ v -> evl v f == False) (allVals f)))

-- OLD CODE, shortened in toCnf
--falseValuations :: Form -> Form
--falseValuations f | tautology f = let name = head $ propNames f in (Dsj [(Prop name), (Neg (Prop name))])
--                  | otherwise   = Cnj (map (\x -> (Dsj (map (\(n,b) -> if b then (Neg (Prop n)) else (Prop n)) x))) (filter (\ v -> evl v f == False) (allVals f)))

--falseValuationsToCnf :: [Valuation] -> Form
--falseValuationsToCnf [] = (Dsj [(Prop 1), (Neg (Prop 1))])
--falseValuationsToCnf xs = Cnj (map (\x -> (Dsj (map (\(n,b) -> if b then (Neg (Prop n)) else (Prop n)) x))) xs)

-- test = falseValuations (Cnj [(Dsj [(Prop 1),(Prop 2)]),(Dsj [(Prop 1),(Neg (Prop 3))]),(Dsj [(Neg (Prop 2)),(Prop 2)]),(Dsj [(Neg (Prop 2)),(Neg (Prop 3))])])