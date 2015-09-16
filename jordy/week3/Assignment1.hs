module Assignment1

where

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
 - Testing:
 - 	contradiction
 - 	> contradiction (Cnj [(Prop 1),(Neg (Prop 1))])
 -	True
 -	> contradiction (Cnj [(Prop 1),(Neg (Prop 2))])
 -	False
 -	> contradiction (Dsj [(Prop 1),(Neg (Prop 1))])
 -	False
 -	> contradiction (Neg (Dsj [(Impl (Prop 1) (Prop 2)), (Impl (Prop 2) (Prop 1))]))
 -	True
 -
 -	tautology
 -	> tautology (Dsj [(Prop 1), (Neg (Prop 1))])
 -	True
 -	> tautology (Dsj [(Prop 1), (Neg (Prop 2))])
 -	False
 -	> tautology (Dsj [(Impl (Prop 1) (Prop 2)), (Impl (Prop 2) (Prop 2))])
 -	True
 -	> tautology (Cnj [(Impl (Prop 1) (Prop 2)), (Impl (Prop 2) (Prop 2))])
 -	
 -	entails
 -	> 
 -}
