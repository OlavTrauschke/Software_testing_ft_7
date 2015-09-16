module Assignment2

where

import Lecture3
import ArbitraryForm

prop_Parse f = [f] == (parse $ show f)
    where types = f::Form

{-
 - Time spent:
 - Understanding parse: 15min
 - Implementing ArbitraryForm: 3hours
 -
 -}