module Assignment4 where

    import Lecture6
    import Assignment3
    import Control.Monad
    import Data.List

    smallestFooler :: (Integer -> IO Bool) -> [Integer] -> IO Integer
    smallestFooler f (n:ns) = do 
                                    p <- f n
                                    if p then return n else smallestFooler f ns

    findSmallestFooler :: Int -> (Integer -> IO Bool) -> [Integer] -> IO Integer
    findSmallestFooler n f ns = liftM minimum (mapM (\_ -> smallestFooler f ns) [1..n])

    {-
     - By running 'findSmallestFooler [n] (prime_tests_F [k]) composites' with n being the number of tests and k the k for the Fermat's Primality Check,
     - we can see what the smallest composite number is which fools the test.
     - With n = 100 we should be able to find a nice smallest composite which fools the primality check. But this test is by no way definitive, as with
     - every test different numbers are tested and the numbers giving the composites away might not always get tested.
     -
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 1) composites
     - 4
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 2) composites
     - 4
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 3) composites
     - 4
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 4) composites
     - 4
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 5) composites
     - 4
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 6) composites
     - 91
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 7) composites
     - 561
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 8) composites
     - 561
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 9) composites
     - 561
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 10) composites
     - 1105
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 11) composites
     - 1729
     - *Assignment4> findSmallestFooler 100 (prime_tests_F 12) composites
     - 1105
     - 
     - What can bee seen is that with k 1-5 the check is not very effective, as the lowest composite to fool the fermat's primality check is 4.
     - From k > 7 the found foolers are in the list of carmichael numbers.
     -}