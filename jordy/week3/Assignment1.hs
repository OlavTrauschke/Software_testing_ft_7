module Assignment1

where

import Data.List
import System.Random
import Lecture3

contradiction :: Form -> Bool
contradiction = not.satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment 
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

{-
 - Time spent on implementation: 10min.
 -
 -}
