-- Lab 3, assignment 3 
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

module Assignment3

where

import Lecture3
import Assignment1
import ArbitraryForm

-- Time spent: 20 minutes
-- Lazy implementation by checking all false valuations, negating those in a disjunction and then conjuncting the disjuctions.
toCnfLazy :: Form -> Form
toCnfLazy f | tautology f = let p = (Prop (head $ propNames f)) in (Dsj [p, (Neg p)]) -- If f is a tautology, there are no false valuations, so we have to create a Form which is always true with a random property form the given Form.
        | otherwise   = Cnj (map (\x -> (Dsj (map (\(n,b) -> if b then (Neg (Prop n)) else (Prop n)) x))) (filter (\ v -> evl v f == False) (allVals f)))
          {- First we filter all the False valuations. These valuations are then mapped.
           - The inner map negates each property of a valuation if needed and added to a disjunction,
           - while the outer map maps over all valuations and joins them in a conjuction.
           -}