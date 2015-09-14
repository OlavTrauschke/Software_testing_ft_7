Toy Examples with QuickCheck

> module ToyQuickCheck where
> import Test.QuickCheck

Suppose we have a function

f :: a -> a -> a
And want to test that it is commutative, i.e.

f x y == f y x
With QuickCheck this is as easy as this:

quickCheckResult (\ x y -> f x y == f y x)
For example, run these:

quickCheckResult (\ x y -> x + y == y + x)

quickCheckResult (\ x y -> x / y == y / x)
Now, suppose we have our own data type and function:

> data Subject  = Mathematics | Philosophy | Anthropology deriving (Eq,Show,Enum)
> compatible :: Subject -> Subject -> Bool
> compatible x y
>  | x == y                  = True
>  | Philosophy `elem` [x,y] = True
>  | otherwise               = False

Is this function commutative?

quickCheckResult (\ x y -> compatible x y == compatible y x)
Ouch, we get:

No instance for (Arbitrary Subject)
arising from a use of ‘quickCheckResult’
We need to tell Haskell how to find an arbitrary

> instance Arbitrary Subject where
>   arbitrary = elements [Mathematics,Philosophy,Anthropology]

Now we can run tests.

> badCompatible :: Subject -> Subject -> Bool
> badCompatible x y
>  | x == y          = True
>  | x == Philosophy = True
>  | otherwise       = False

This does not pass the test for commutativity. Besides a lot of debugging info, QuickCheck also gives us a counter example:

*ToyQuickCheck GOA> quickCheckResult (\ x y -> badCompatible x y == badCompatible y x)
*** Failed! Falsifiable (after 5 tests):
Philosophy
Mathematics
Failure {numTests = 5, numShrinks = 0, numShrinkTries = 0, numShrinkFinal = 0, usedSeed = TFGenR 0000000E32E47C5700000000000F4240000000000000DFBB00000046C7CFE000 0 62 6 0, USEDSIZE = 4, REASON = "FALSIFIABLE", THEEXCEPTION = NOTHING, LABELS = [], OUTPUT = "*** FAILED! FALSIFIABLE (AFTER 5 TESTS): \NPHILOSOPHY\NMATHEMATICS\N"}
Links

The QuickCheck package

The QuickCheck manual. This is not up-to-date, but useful nevertheless.

Intro to QuickCheck1

Intro to QuickCheck2

A more complex example of using QuickCheck