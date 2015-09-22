module Assignment8

where

import Rel
import Assignment4
import Assignment5
import Test.QuickCheck
import Control.Monad
import Data.List

prop_TrSym_vs_SymTr :: Rel Int -> Bool
prop_TrSym_vs_SymTr r = (symClos.trClos) r == (trClos.symClos) r

{-
 - *Assignment8> quickCheck prop_TrSym_vs_SymTr 
 - *** Failed! Falsifiable (after 3 tests):  
 - [(1,2),(3,3)]
 -
 - Which makes sense.
 - The symmetric closure of [(1,2),(3,3)] = [(1,2),(2,1),(3,3)], and the transitive closure of [(1,2),(2,1),(3,3)] = [(1,1),(1,2),(2,1),(3,3)]
 - While the transitive closure of [(1,2),(3,3)] = [(1,2),(3,3)], and the symmetric clusure of [(1,2),(3,3)] = [(1,2),(2,1),(3,3)]

 - Thus (symClos.trClos) ≠ (trClos.symClos)
 -
 -}