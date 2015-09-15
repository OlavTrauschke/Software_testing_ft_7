-- Lab 3, assignment 3
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

module Assignment3 where

import Data.List
import System.Random
import Lecture3
import Assignment1

-- Push disjunctions downward
-- (A & B) | C  =>  (A | C) & (B | C)
dsj_push :: Form -> Form 
dsj_push (Dsj [Cnj [a, b], c]) = Cnj [dsj_push (Dsj [a, c]), dsj_push (Dsj [b, c])]
dsj_push (Dsj [a, Cnj [b, c]]) = Cnj [dsj_push (Dsj [a, b]), dsj_push (Dsj [a, c])]
dsj_push (Cnj [a, b]) = Cnj [dsj_push a, dsj_push b] 
dsj_push (Dsj [a, b]) = Dsj [dsj_push a, dsj_push b]
dsj_push a = a

loop_dsj_push :: Form -> Form
loop_dsj_push f
	| f2 == f	= f
	| otherwise	= loop_dsj_push f2
	where f2 = dsj_push f


toCnf :: Form -> Form
toCnf f = loop_dsj_push . nnf . arrowfree $ f














