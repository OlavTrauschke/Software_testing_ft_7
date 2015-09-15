module Assignment2

where

import Data.List
import System.Random
import Lecture3
import Assignment1
import Test.QuickCheck
import Control.Applicative
import Control.Monad
import ArbitraryForm

prop_Parse f = [f] == (parse $ show f)
    where types = f::Form

{-
 - Time spent:
 - Understanding parse: 15min
 - Implementing ArbitraryForm: 3hours
 -
 -}