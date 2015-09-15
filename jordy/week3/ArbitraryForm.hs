module ArbitraryForm

where

import Lecture3
import Test.QuickCheck
import Control.Monad

instance Arbitrary Form where
    arbitrary = sized arbForm

arbForm 0 = liftM Prop (choose (1,3))
arbForm n = frequency
    [ (1, liftM Prop (choose (1,3)))
    , (2, liftM Neg (subForm))
    , (3, liftM Cnj (vectorOf 2 (resize (n `div` 2) arbitrary)))
    , (3, liftM Dsj (vectorOf 2 (resize (n `div` 2) arbitrary)))
    , (2, liftM2 Impl (subForm)
                     (subForm))
    , (2, liftM2 Equiv (subForm)
                     (subForm)) ]
    where subForm = arbForm (n `div` 2)