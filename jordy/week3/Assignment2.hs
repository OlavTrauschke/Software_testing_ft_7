module Assignment2

where

import Data.List
import System.Random
import Lecture3
import Test.QuickCheck
import Control.Applicative
import Control.Monad

instance Arbitrary Form where
	arbitrary = sized arbForm

arbForm 0 = liftM Prop (choose (1,10))
arbForm n = frequency
    [ (1, liftM Prop (choose (1,10)))
    , (2, liftM Neg (arbForm (n `div` 2)))
    , (2, liftM Cnj (vectorOf 2 (resize (n `div` 2) arbitrary)))
    , (2, liftM Dsj (vectorOf 2 (resize (n `div` 2) arbitrary)))
    , (2, liftM2 Impl (arbForm (n `div` 2))
                     (arbForm (n `div` 2)))
    , (2, liftM2 Equiv (arbForm (n `div` 2))
                     (arbForm (n `div` 2))) ]

test = quickCheck (\(NonEmpty x) -> x == parse (show x))

{-
 - Time spent:
 - 	Understanding parse: 15min
 -	
 -
 -}

