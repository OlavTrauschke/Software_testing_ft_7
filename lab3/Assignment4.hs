-- Lab 3, assignment 4
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

module Assignment4

where

import Lecture3
import Assignment1
import Assignment3
import ArbitraryForm

prop_Cnf f = equiv f (toCnf f)
prop_LazyCnf f = equiv f (toCnfLazy f)

{-
 - 
 - Our test method uses QuickCheck. The arbitrary implementation can be found in ArbitraryForm.hs
 - The first test checks if the CNF of the formula is equivalent to the original formula.
 -
 -}
