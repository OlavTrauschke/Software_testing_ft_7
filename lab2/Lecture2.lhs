Lecture 2: Preconditions and Postconditions

> module Lecture2
> 
> where 
> 
> import System.Random

Tools for Test Generation: Random Numbers

Random number generation. Getting a random integer:

> getRandomInt :: Int -> IO Int
> getRandomInt n = getStdRandom (randomR (0,n))

This gives:

 *Lecture2> :t getRandomInt
 getRandomInt :: Int -> IO Int
 *Lecture2> getRandomInt 20
 2
 *Lecture2> getRandomInt 20
 11
 *Lecture2> getRandomInt 20
 8
And next time you do it you get different values.

Random Integer Lists

Randomly flipping the value of an Int:

> randomFlip :: Int -> IO Int
> randomFlip x = do 
>    b <- getRandomInt 1
>    if b==0 then return x else return (-x)

Random integer list:

> genIntList :: IO [Int]
> genIntList = do 
>   k <- getRandomInt 20
>   n <- getRandomInt 10
>   getIntL k n
> 
> getIntL :: Int -> Int -> IO [Int]
> getIntL _ 0 = return []
> getIntL k n = do 
>    x <-  getRandomInt k
>    y <- randomFlip x
>    xs <- getIntL k (n-1)
>    return (y:xs)

*Lecture2> genIntList
[-17,-13,1]
*Lecture2> genIntList
[-13,12,16]
*Lecture2> genIntList
[0,-3,4,2,-2,0,1,3]
*Lecture2> genIntList
[-8,5,8,4,5,2,0,-10,10,10]
*Lecture2> genIntList
[4,-1,-1,17,17,0,-3,8]
*Lecture2> genIntList
[10,-10,6,9,2,-2,-4]
*Lecture2> genIntList
[1,5,5,2,2,-2,-3,-9,-13]
The definitions above use do notation for monadic programming, but for now you need not worry about the details.

Test Properties

Let a be some type.

Then a -> Bool is the type of a properties.

An a property is a function for classifying a objects.

Properties can be used for testing, as we will now explain.

Preconditions and postconditions of functions

Let f be a function of type a -> a.

A precondition for f is a property of the input.

A postcondition for f is a property of the output.

Hoare Statements, or Hoare Triples

{p} f {q}.

Intended meaning: if the input x of f satisfies p, then the output f x of f satisfies q.

Examples (assume functions of type ):

{even}(λx↦x+1) {odd}.

{odd}(λx↦x+1) {even}.

{⊤}(λx↦2x) {even}.

{⊥}(λx↦2x) {odd}.

Tony Hoare (born 1934)
Tony Hoare (born 1934)

Tony Hoare is famous for Hoare Logic, he is a pioneer of process calculi with his Communicating Sequential Processes, and he is the inventor of a famous efficient sorting algorithm called QuickSort. Hoare was the winner of the 1980 Turing Award.

Quicksort

Quicksort is a very early example where recursion is used as the key to a very efficient sorting algorithm. Here is an implementation in Haskell:

> quicksort :: Ord a => [a] -> [a]  
> quicksort [] = []  
> quicksort (x:xs) = 
>    quicksort [ a | a <- xs, a <= x ]  
>    ++ [x]
>    ++ quicksort [ a | a <- xs, a > x ]

The quicksort function has the property that it turns any finite list of items into an ordered list of items. So in this case the precondition is the property isTrue that holds of any list (in fact of anything at all):

> isTrue :: a -> Bool
> isTrue _ = True

and the postcondition is:

> prop_ordered :: Ord a => [a] -> Bool
> prop_ordered [] = True
> prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

So we have the following Hoare triple:

{ isTrue xs } ys = quicksort xs { prop_ordered ys }
What this means: if xs is any (finite) list, and ys is the result of the call quicksort xs, then ys is an ordered list.

This Hoare triple for quicksort can be used for automated testing.

Automated test generation

> testR :: Int -> Int -> ([Int] -> [Int])
>                     -> ([Int] -> [Int] -> Bool) -> IO ()
> testR k n f r = if k == n then print (show n ++ " tests passed")
>                 else do
>                   xs <- genIntList
>                   if r xs (f xs) then
>                     do print ("pass on: " ++ show xs)
>                        testR (k+1) n f r
>                   else error ("failed test on: " ++ show xs)

Here is a function for running 100 tests with a given postcondition.

> testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
> testPost f p = testR 1 100 f (\_ -> p)

Example use

 testPost quicksort prop_ordered
Let's compare quicksort with the following:

> quicksrt :: Ord a => [a] -> [a]  
> quicksrt [] = []  
> quicksrt (x:xs) = 
>    quicksrt [ a | a <- xs, a < x ]  
>    ++ [x]
>    ++ quicksrt [ a | a <- xs, a > x ]

We can test this as well:

 testPost quicksrt prop_ordered
Is there a postcondition property that quicksort has but quicksrt lacks?

> samelength :: [Int] -> [Int] -> Bool
> samelength xs ys = length xs == length ys
> testRel :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
> testRel f r = testR 1 100 f r 

Use this as follows:

testRel quicksrt samelength 
Think of some more properties and relations that can be used to test quicksort and quicksrt.

Next, find some relevant properties to test reverse.

Precondition strengthening

If

{ p } f { q }
holds, and p' is a property that is stronger than p, then

{ p' } f { q }
holds as well. This is called precondition strenghtening. In order to see what this principle means, we need a good understanding of the relation stronger than. See workshop 2.

A property that is stronger than isTrue is the property of being different from the empty list. In Haskell, we can express this as not.null.

Thus, precondition strengthening allows us to derive the following Hoare triple:

{ not.null xs } ys = quicksort xs { sorted ys }           
Postcondition weakening

If

{ p } f { q }
holds, and q' is a property that is weaker than q, then

{ p } f { q' }
holds as well. This is called postcondition weakening. Again, to understand what this means we need a good understanding of what weaker than means.

Hoare triples as contracts

Hoare triples can be viewed as a special case of the contracts used as specifications in design by contract software development.

The preconditions specify what the contract expects. In the case of quicksort, the contract expects nothing. The postconditions express what the contract guarantees. In the case of quicksort, the contract guarantees that the output of the program is an ordered list.

Useful logic notation

> infix 1 ==> 
> 
> (==>) :: Bool -> Bool -> Bool
> p ==> q = (not p) || q
> forall = flip all

Stronger and Weaker as Predicates on Test Properties

Stronger than$ and weaker than* are relations on the class of test properties.

Here are the implementations. These assume that we restrict to a finite domain given by a [a].

> stronger, weaker :: [a] -> 
>        (a -> Bool) -> (a -> Bool) -> Bool
> stronger xs p q = forall xs (\ x -> p x ==> q x)
> weaker   xs p q = stronger xs q p 

Note the presence of the [a] argument for the test domain.

The Weakest and the Strongest Property

Use ⊤ for the property that always holds. This is the weakest possible property. Implementation: _ -> True. See isTrue above.

Use ⊥ for the property that never holds. This is the strongest property. Implementation: _ -> False.

Everything satisfies _ -> True.

Nothing satisfies _ -> False.

Negating a Property

> neg :: (a -> Bool) -> a -> Bool
> neg p = \ x -> not (p x)

But there is a simpler version:

(not.) = \ p -> not . p =  \ p x -> not . (p x)
Conjunctions and Disjunctions of Properties

> infixl 2 .&&. 
> infixl 2 .||.
> (.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
> p .&&. q = \ x -> p x && q x 
> 
> (.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
> p .||. q = \ x -> p x || q x

Review questions: What is the difference between (&&) and .(&&). ? What is the difference between (||) and .(||). ?

Examples

*Lecture2> stronger [0..10] even (even .&&. (>3))
False
*Lecture2> stronger [0..10] even (even .||. (>3))
True
*Lecture2> stronger [0..10] (even .&&. (>3)) even
True
*Lecture2> stronger [0..10] (even .||. (>3)) even
False       
    
Further exercises with this: workshop of today.

Importance of Precondition Strengthening for Testing

If you strengthen the requirements on your inputs, your testing procedure gets weaker.

Reason: the set of relevant tests gets smaller.

Note: the precondition specifies the relevant tests.

Preconditions should be as weak as possible (given a function and a postcondition).

Importance of Postcondition Weakening for Testing

If you weaken the requirements on your output, your testing procedure gets weaker.

Reason: the requirements that you use for checking the output get less severe.

Note: the postcondition specifies the strength of your tests.

Postconditions should be as strong as possible (given a function and a precondition).

Falsifiability, Critical Rationalism, Open Society

Karl Popper (1902--1992
Karl Popper (1902--1992

Karl Popper stated the famous principle of falsifiability:

To say something meaningful, say something that can turn out false.

Pre- and Postcondition Composition Rule

From

{ p } f { q }     and    { q } g { r }
conclude:

{ p } g . f { r }
This gives an important way to derive useful specifications for compositions from specifications for their parts.
More about this in the next lecture.

Next time

More about applications of the composition rule.

More about the connections between functional and imperative programming.

More about test generation and tools for test generation.