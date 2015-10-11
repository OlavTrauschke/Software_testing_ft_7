module Assignment5 where

    import Lecture6
    import Assignment4

    carmichael :: [Integer]
    carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
        k <- [2..], 
        isPrime (6*k+1), 
        isPrime (12*k+1), 
        isPrime (18*k+1) ]

    {-
     - By running 'findSmallestFooler [n] (prime_tests_F [k]) carmichael' with n being the number of tests and k the k for the Fermat's Primality Check,
     - we can see what the smallest carmichael number is which fools the test.
     - With n = 100 we should be able to find a smallest carmichael number which fools the primality check.
     -
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 1) carmichael
     - 294409
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 2) carmichael
     - 294409
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 3) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 4) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 5) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 6) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 7) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 8) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 9) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 10) carmichael
     - 294409 
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 20) carmichael
     - 294409
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 50) carmichael
     - 294409
     - *Assignment5> findSmallestFooler 100 (prime_tests_F 100) carmichael
     - 56052361
     -
     - With these numbers, fermat's primality check is pretty much useless, even with k = 50, it still failes to mark the first number as composite.
     - Which is to be expected, reading the wikipedia page and KhanAcademy.
     - https://www.khanacademy.org/computing/computer-science/cryptography/random-algorithms-probability/v/fermat-primality-test-prime-adventure-part-10
     -}