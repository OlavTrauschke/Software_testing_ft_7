module Assignment3

where

import Lecture3
import ArbitraryForm

--Exercise 1 below. We spent about an hour on this exercise

-- |whether a logical formula is a contradiction
{-Tested by verifying that
contradiction (Cnj [(Prop 1),(Neg (Prop 1))]) is true,
contradiction (Dsj [(Prop 1),(Neg (Prop 1))]) is false-}
contradiction :: Form -> Bool
contradiction = not.satisfiable

-- |whether a logical formula is a tautology
--The function below was based strongly on satisfiable
{-Tested by verifying that
tautology (Dsj [(Prop 1),(Neg (Prop 1))]) is true,
tautology (Cnj [(Prop 1),(Neg (Prop 1))]) is false-}
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- |logical entailment
{-Tested by verifying that
entails (Prop 1) (Prop 1) is true,
entails (Neg (Prop 1)) (Neg (Prop 1)) is true,
entails (Prop 1) (Neg (Prop 1)) is false,
entails (Neg (Prop 1)) (Prop 1) is false-}
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

-- |logical equivalence
{-Tested by verifying that
equiv (Prop 1) (Prop 1) is true,
equiv (Impl (Prop 1) (Prop 2)) (Dsj [(Prop 2),(Neg (Prop 1))]) is true,
equiv (Prop 1) (Neg (Prop 1)) is false-}
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

--Exercise 2 below. We spent about an hour and fifteen minutes on this exercise.

{-Tested first of all by verifying that all symbols parse by checking that
parse "* (1 2 3)" is printed as [*(1 2 3)],
parse "+ (1 2 3)" is printed as [+(1 2 3)],
parse "-1" is printed as [-1],
parse "(1 ==> 2)" is printed as [(1==>2)],
and "(1 <=> 2)" is printed as [(1<=>2)].
Furthermore, we tested by verifying that a longer formula, combining implication,
equivalence, disjunction (which could as well have been conjunction obviously) and negation
parsed right: parse "((1 ==> 2) <=> + (2 -1))" is printed as [((1==>2)<=>+(2 -1))]-}

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

-- |test property for toCnf by Jordy
prop_Cnf f = equiv f (toCnf f)

--Exercise 5 below. We spent about two and a half hours on this exercise.
type Clause = [Int]
type Clauses = [Clause]

-- |convert any Form to Clauses
form2cls :: Form -> Clauses
form2cls = cnf2cls.toCnf

-- |convert a Form in cnf to Clauses
-- tested by running check prop_Cls
cnf2cls :: Form -> Clauses
cnf2cls (Cnj x) = map toClause x
cnf2cls x = [(toClause x)]

-- |convert a dsj or a literal to a clause
toClause :: Form -> Clause
toClause (Dsj x) = map toInt x
toClause x = [(toInt x)]

-- |convert a literal (wrapped in a form) to an int
toInt :: Form -> Int
toInt (Prop x) = x
toInt (Neg (Prop x)) = -x

-- |general test property for testing cnf2cls
-- combines clsAsLongAsCnf and clsElemsAsLongAsCnfElems
prop_Cls :: Form -> Bool
prop_Cls f = (clsAsLongAsCnf f) && (clsElemsAsLongAsCnfElems f)

-- |property for testing
-- true when the number of elements in the cnf of a Form
-- equals the number of elements in it's Clauses form
clsAsLongAsCnf :: Form -> Bool
clsAsLongAsCnf f = length (splitCnj g) == length (cnf2cls g)
  where g = toCnf f

-- |property for testing
-- true when the number of elements in the elements of the cnf of a Form
-- match the number of elements in the elements of it's Clauses form
clsElemsAsLongAsCnfElems :: Form -> Bool
clsElemsAsLongAsCnfElems f = all (\x -> length (splitDsj (h !! x)) == length (k !! x)) [0..(length h)-1]
  where g = toCnf f
        h = splitCnj g
        k = cnf2cls g