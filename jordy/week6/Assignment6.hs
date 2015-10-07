module Assignment6 where

    import Control.Monad
    import Lecture6

    {-
     - The function probablyPrimeListGen generates a list of length n, with (probably) primes given the generator and input list.
     - Variable 'n' is the number of (posibly) primes to generate.
     - Variable 'k' is how many tests the primeMR function should do, more makes the list more reliable, but is also more resource heavy
     - Variable 'f' is a transformation on the list of Integers of variable 'ps'
     - Variable 'ps' is the list of integers on which 'f' will be used and finally tested
     - Output is an IO list of integers of the (posibly) prime numbers found.
     -}
    probablyPrimeListGen :: Int -> Int -> (Integer -> Integer) -> [Integer] -> IO [Integer]
    probablyPrimeListGen 0 _ _ _      = return []
    probablyPrimeListGen n k f (p:ps) = let 
                                            mp = f p
                                        in do
                                            r <- primeMR k mp
                                            if r then do
                                                liftM (mp:) (probablyPrimeListGen (n-1) k f ps)
                                            else
                                                probablyPrimeListGen n k f ps

    {- A function to search for 'n' (probably) mersenne primes -}
    probablyMersennePrimes :: Int -> Int -> IO [Integer]
    probablyMersennePrimes n k = probablyPrimeListGen n k (\p -> 2^p-1) primes

    {- A function to search for 'n' (probably) primes -}
    probablyPrimes :: Int -> Int -> IO [Integer]
    probablyPrimes n k = probablyPrimeListGen n k id [1..]

    {- A function to check if the generator generates the right mersenne primes with a given 'k' -}
    {- Returns true most of the time even with a 'k' of 1 -}
    genCheck :: Int -> IO Bool
    genCheck k = let
                    pmps = probablyMersennePrimes 15 k
                    mps = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15]
                 in
                    liftM (mps ==) pmps