-- Lab 3, assignment 3
-- Team: FT_7
-- Date: 10.09.2015
-- Software Testing, Master Software Engineering (2015), University Of Amsterdam

module Assignment3 where

import Data.List
import System.Random
import Lecture3



-- nnf.arrowfree
-- Dsj [Dsj [Cnj [p, Neg q], Cnj [q, Neg r]] , Dsj [Neg p, r]]


-- Cnj [Dsj [p, q, Neg p, r], Dsj [p, Neg r, Neg p, r], Dsj [Neg q, q, Neg p, r], Dsj [Neg q, Neg r, Neg p, r]]



cnf :: Form -> Form 
cnf (Dsj [Cnj [a, b], c]) = Cnj [cnf (Dsj [a, c]), cnf (Dsj [b, c])]
cnf (Dsj [a, Cnj [b, c]]) = Cnj [cnf (Dsj [a, b]), cnf (Dsj [a, c])]

cnf (Cnj [a, b]) = Cnj [cnf a, cnf b]
cnf (Dsj [a, b]) = Dsj [cnf a, cnf b]
cnf a = a



