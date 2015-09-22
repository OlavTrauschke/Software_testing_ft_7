module Rel

where

import Data.List
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

instance Arbitrary (Rel Int) where
    arbitrary = liftM (sort.nub) $ listOf $ smallInt >*< smallInt

--arbRel :: Gen (Rel Int) 
--arbRel = liftM (sort.nub) $ listOf $ smallInt >*< smallInt

smallInt :: Gen Int
smallInt = choose (1,5)

smallIntTuple :: Gen (Int, Int)
smallIntTuple = (>*<) smallInt smallInt

-- From https://hackage.haskell.org/package/checkers-0.4.3/src/src/Test/QuickCheck/Instances/Tuple.hs
(>*<) :: Gen a -> Gen b -> Gen (a,b)
x >*< y = liftM2 (,) x y