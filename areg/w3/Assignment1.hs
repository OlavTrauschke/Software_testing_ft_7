-- Lab 3, assignment 1
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

module Assignment1 where

import Data.List
import System.Random
import Lecture3

{-
We spended about 2 hours on this assignment. We tested the definitions using example propositions with known outcomes.

The unary properties can be tested with:
> test_unary tautology forms_tautology 

The binary properties can be tested with:
> test_binary entails forms_entails

-}

-- Test unary property of a proposition (eg. contradiction, tautology)
test_unary :: (Form -> Bool) -> [(Form, Bool)] -> IO()
test_unary _ [] = print "Passed all test forms!"
test_unary property (f:fs)
	| (property $ fst f) == snd f		= test_unary property fs
	| otherwise				= error $ "failed test on: " ++ (show f)	

-- Test binary property of a proposition (eg. entailment, equivalence)
test_binary :: (Form -> Form -> Bool) -> [((Form, Form), Bool)] -> IO()
test_binary _ [] = print "Passed all test forms!"
test_binary property (f:fs)
	| (property (fst (fst f)) (snd (fst f))) == snd f	= test_binary property fs
	| otherwise						= error $ "failed test on: " ++ (show f)	



--------------------------------------------------------------------
contradiction :: Form -> Bool
contradiction = not.satisfiable

-- Examples of contradictions
forms_contradiction :: [(Form, Bool)]
forms_contradiction = 
	[(Cnj [p, (Neg p)], True),
	(Cnj [Impl p q, p, Neg q], True),
	(Cnj [(Prop 1),(Neg (Prop 1))], True),
	(Dsj [(Prop 1),(Neg (Prop 1))], False), 
	(Cnj [(Prop 1),(Neg (Prop 1))], True),
	(Cnj [(Prop 1),(Neg (Prop 2))], False),
	(Dsj [(Prop 1),(Neg (Prop 1))], False),
	(Neg (Dsj [(Impl (Prop 1) (Prop 2)), (Impl (Prop 2) (Prop 1))]), True)]

--------------------------------------------------------------------
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Examples of tautologies
forms_tautology :: [(Form, Bool)]
forms_tautology = 
	[(Dsj [p, (Neg p)], True),
	(Equiv (Impl p q) (Dsj [Neg p, q]), True),
	(Dsj [(Prop 1),(Neg (Prop 1))], True),
	(Cnj [(Prop 1),(Neg (Prop 1))], False),
	(Dsj [(Prop 1), (Neg (Prop 1))], True),
	(Dsj [(Prop 1), (Neg (Prop 2))], False),
	(Dsj [(Impl (Prop 1) (Prop 2)), (Impl (Prop 2) (Prop 2))], True),
	(Cnj [(Impl (Prop 1) (Prop 2)), (Impl (Prop 2) (Prop 2))], False)]


--------------------------------------------------------------------
-- | logical entailment 
-- "P |= Q" means "|= P -> Q" means "P -> Q is a tautology"
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- Examples of logical entailments
forms_entails :: [((Form, Form), Bool)]
forms_entails = 
	[((Cnj [Impl p (Dsj [q, r]), Dsj [Neg r, Neg p], Neg p ], Neg p), True),
	((Cnj [Equiv p q, Neg (Cnj [q, Neg p]), q ], p), True),
	((Prop 1, Prop 1), True),
	(((Neg (Prop 1)), (Neg (Prop 1))), True),
	(((Prop 1), (Neg (Prop 1))), False),
	(((Neg (Prop 1)), (Prop 1)), False)]


--------------------------------------------------------------------
-- | logical equivalence
-- "P is equivalent to Q" means "P |= Q & Q |= P", so "|= P <-> Q"
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Examples of logical equivalences
forms_equiv :: [((Form, Form), Bool)]
forms_equiv = 
	[((Impl p q, Dsj [Neg p, q]), True),
	((Cnj [Impl p q, Impl p r], Impl p (Cnj [q, r])), True),
	(((Prop 1), (Prop 1)), True),
	(((Impl (Prop 1) (Prop 2)), (Dsj [(Prop 2),(Neg (Prop 1))])), True),
	(((Prop 1), (Neg (Prop 1)) ), False)]





