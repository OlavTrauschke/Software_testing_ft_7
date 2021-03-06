module Assignment4

where

import Lecture3
import Assignment1
import Assignment3
import ArbitraryForm

prop_CnfEquiv f = equiv f (toCnf f)

prop_CnfDef m f = isInCnf $ m f

isInCnf :: Form -> Bool
-- CNF can be a single property
isInCnf (Prop _) = True
-- CNF can be a conjuction, for which all sub forms have to be either a disjunction, or a property, and lastly the disjuntion consists of either properties, or their negation.
isInCnf (Cnj xs) = all (\x -> case x of {(Dsj ys) -> all (\y -> case y of {(Prop _) -> True; (Neg (Prop _)) -> True; _ -> False}) ys; (Prop _) -> True; _ -> False}) xs
-- CNF can also be a disjunction of two properties, or their negation.
isInCnf (Dsj xs) = length xs <= 2 && all (\x -> case x of {(Prop _) -> True; (Neg (Prop _)) -> True; _ -> False}) xs
-- Otherwise not CNF.
isInCnf _ = False

{-
 - 
 - Our test method uses QuickCheck. The arbitrary implementation can be found in ArbitraryForm.hs
 - The first test checks if the CNF of the formula is equivalent to the original formula. (Usage "check $ prop_CnfEquiv [toCnf | toCnfLazy]")
 - The second test checks if the form of the CNF. (Usage "check $ prop_CnfDef [toCnf | toCnfLazy]")
 -
 -}
