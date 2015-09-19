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

--Exercise 3 below. We spent about 2,5 hours on this exercise.

-- |convert a formula of propositional logic to conjunctive normal form
-- Tested by running check prop_Cnf
toCnf :: Form -> Form
toCnf = arrowfree # nnf # cnf

-- |convert a formula of propositional logic in negation normal form to conjunctive normal form 
cnf :: Form -> Form
cnf (Prop x) = (Prop x)
cnf (Neg (Prop x)) = (Neg (Prop x))
cnf (Dsj f)
  | any (\ x -> isCnj x) f = Cnj (toCnjs (split (splitDsj) (map cnf f)))
  | otherwise = Dsj (split (splitDsj) (map cnf f))
cnf (Cnj f) = Cnj (split (splitCnj) (map cnf f))

-- |whether a Form is a conjunction
isCnj :: Form -> Bool
isCnj (Cnj _) = True
isCnj _ = False

-- |split elements in a list of Forms using a provided split function
split :: (Form -> [Form]) -> [Form] -> [Form]
split _ [] = []
split f (x:xs) = (f x) ++ (split f xs)

-- |get the list of elements a conjunction reaches over. Does nothing with formulas which aren't cnjs
splitCnj :: Form -> [Form]
splitCnj (Cnj f) = f
splitCnj x = [x]

-- |get the list of elements a disjunction reaches over. Does nothing with formulas which aren't dsjs
splitDsj :: Form -> [Form]
splitDsj (Dsj f) = f
splitDsj x = [x]

-- |get a list of Forms that should be written conjunctively from a list of Froms that should be written disjunctively
{-the use of sequence in this function was inspired by the response of newacct posted on Nov 7 '10, edited by
JB on Feb 10 '11 as found on September 15 '15 at http://stackoverflow.com/questions/4119730/cartesian-product-}
toCnjs :: [Form] -> [Form]
toCnjs = toDsjs.sequence.toListsOfElementsOfConjunctions

-- |get lists of all elements of conjunctions in a list of conjunctions and literals
toListsOfElementsOfConjunctions :: [Form] -> [[Form]]
toListsOfElementsOfConjunctions [] = []
toListsOfElementsOfConjunctions ((Prop x):xs) = [(Prop x)]:(toListsOfElementsOfConjunctions xs)
toListsOfElementsOfConjunctions ((Neg (Prop x)):xs) = [(Neg (Prop x))]:(toListsOfElementsOfConjunctions xs)
toListsOfElementsOfConjunctions ((Cnj x):xs) = x:toListsOfElementsOfConjunctions xs

-- |convert a list of lists of which the elements repersent disjunct elements into a list of disjunctions
toDsjs :: [[Form]] -> [Form]
toDsjs [] = []
toDsjs (x:xs) = (Dsj x):(toDsjs xs)