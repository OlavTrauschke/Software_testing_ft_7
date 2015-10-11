module Assignment3 where

    import Lecture6

    {-
     - The generation of an infinite list of composites is just all natural numbers with the primes filtered out.
     - This method should be fast enough for our purposes.
     -}

    composites :: [Integer]
    composites = filter (not.isPrime) [0..] 