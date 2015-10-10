module Assignment5b where

     import Lecture6
     import Assignment1
     import Assignment4
     import Assignment5

    {-
     - By running 'findSmallestFooler [n] (primeMR [k]) carmichael' with n being the number of tests and k the k for the Miller-Rabin's Primality Check,
     - we can see what the smallest carmichael number is which fools the test.
     - With n = 1000 we should be able to find a smallest carmichael number which fools the primality check.
     -
     - *Assignment5> findSmallestFooler 100 (primeMR 1) carmichael
     - 294409
     - *Assignment5> findSmallestFooler 100 (primeMR 2) carmichael
     - 294409
     - *Assignment5> findSmallestFooler 100 (primeMR 3) carmichael
     - 228842209 
     - *Assignment5> findSmallestFooler 5 (primeMR 4) carmichael
     - 5028283200968991400321
     - *Assignment5> findSmallestFooler 1 (primeMR 5) carmichael
     - << Apears to never end. >>
     -
     - The Miller-Rabin Primality Check is much better at marking primes from the list of carmichael numbers. It is still not fool proof if
     - the value of k is low. But with a k as low as 4, it is already marking a lot of carmichael numbers correctly.
     -}