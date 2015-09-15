module Assignment4

where

import Data.List
import System.Random
import Lecture3
import Assignment1
import Assignment3
import ArbitraryForm
import Test.QuickCheck
import Control.Applicative
import Control.Monad

test = quickCheckWith stdArgs { maxSize = 15 } prop_Cnf
verboseTest = verboseCheckWith stdArgs { maxSize = 15 } prop_Cnf

prop_Cnf f = equiv f (toCnf f)