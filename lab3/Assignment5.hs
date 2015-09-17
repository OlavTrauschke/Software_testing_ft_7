-- Lab 3, assignment 5
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

module Assignment5

where

import Lecture3
import Assignment1
import Assignment3
import ArbitraryForm

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

-- |get the list of elements a conjunction reaches over. Does nothing with formulas which aren't cnjs
splitCnj :: Form -> [Form]
splitCnj (Cnj f) = f
splitCnj x = [x]

-- |get the list of elements a disjunction reaches over. Does nothing with formulas which aren't dsjs
splitDsj :: Form -> [Form]
splitDsj (Dsj f) = f
splitDsj x = [x]

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