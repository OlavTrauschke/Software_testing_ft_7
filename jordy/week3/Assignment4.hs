module Assignment4

where

import Lecture3
import Assignment1
import Assignment3
import ArbitraryForm

prop_Cnf f = equiv f (toCnf f)