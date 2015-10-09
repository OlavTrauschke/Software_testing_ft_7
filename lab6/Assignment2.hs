module Assignment2 where

    import Lecture6
    import Assignment1
    import Criterion.Main

    {-
     - While testing the performance of our different implementations of the modular exponentiation function, some interesting results arrised.
     - The implementation from Jordy and Floris are much alike, as they are both from the same idea. But the implementation of floris does seem
     - slightly faster in all usecases described here.
     - Finaly the original implementation is in the smallest cases quicker than any of our implementations, this is expected, as our implementations
     - have much more to calculate, and squaring and dividing is not that much of a problem on smaller numbers. When numbers get bigger though, is
     - when this implementation breaks down. While our implementation does not even take twice as much time calculating m7^m8 mod m9 over m6^m7 mod m8,
     - the original implementation is four times slower. The next calculation is even so expensive, that haskell crashes before it can return a value,
     - due to being out of memory. And m6 and m7 are not even close to a prime of 1024 bits, which is (at least) needed for a safe private key.
     - (https://www.google.nl/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=rsa+minimum+key+size)
     -}

    benchmark = defaultMain [
        bgroup "exMJordy"  [ bench "m1^m2 mod m3"  $ nf (exMJ m1 m2) m3,
                             bench "m4^m5 mod m6"  $ nf (exMJ m4 m5) m6,
                             bench "m5^m6 mod m7"  $ nf (exMJ m5 m6) m7,
                             bench "m6^m7 mod m8"  $ nf (exMJ m6 m7) m8,
                             bench "m7^m8 mod m9"  $ nf (exMJ m7 m8) m9
                           ],
        bgroup "exMFloris" [ bench "m1^m2 mod m3"  $ nf (exMF m1 m2) m3,
                             bench "m4^m5 mod m6"  $ nf (exMF m4 m5) m6,
                             bench "m5^m6 mod m7"  $ nf (exMF m5 m6) m7,
                             bench "m6^m7 mod m8"  $ nf (exMF m6 m7) m8,
                             bench "m7^m8 mod m9"  $ nf (exMF m7 m8) m9
                           ],
        bgroup "expMOrg"   [ bench "m1^m2 mod m3"  $ nf (expM m1 m2) m3,
                             bench "m4^m5 mod m6"  $ nf (expM m4 m5) m6,
                             bench "m5^m6 mod m7"  $ nf (expM m5 m6) m7,
                             bench "m6^m7 mod m8"  $ nf (expM m6 m7) m8,
                             bench "m7^m8 mod m9"  $ nf (expM m7 m8) m9
                           ]
        ]

    {-

        *Assignment2> benchmark
        benchmarking exMJordy/m1^m2 mod m3
        Warning: Couldn't open /dev/urandom
        Warning: using system clock for seed instead (quality will be lower)
        time                 9.146 us   (9.006 us .. 9.305 us)
                             0.998 R²   (0.997 R² .. 0.999 R²)
        mean                 9.103 us   (9.006 us .. 9.217 us)
        std dev              353.8 ns   (278.9 ns .. 471.8 ns)
        variance introduced by outliers: 48% (moderately inflated)

        benchmarking exMJordy/m4^m5 mod m6
        time                 42.60 us   (41.63 us .. 43.75 us)
                             0.995 R²   (0.993 R² .. 0.997 R²)
        mean                 43.93 us   (43.01 us .. 45.02 us)
        std dev              3.389 us   (2.706 us .. 4.239 us)
        variance introduced by outliers: 75% (severely inflated)

        benchmarking exMJordy/m5^m6 mod m7
        time                 56.98 us   (55.02 us .. 58.70 us)
                             0.995 R²   (0.993 R² .. 0.998 R²)
        mean                 56.02 us   (55.28 us .. 57.06 us)
        std dev              3.065 us   (2.353 us .. 3.810 us)
        variance introduced by outliers: 59% (severely inflated)

        benchmarking exMJordy/m6^m7 mod m8
        time                 63.72 us   (61.96 us .. 65.49 us)
                             0.996 R²   (0.993 R² .. 0.998 R²)
        mean                 63.44 us   (62.49 us .. 64.55 us)
        std dev              3.263 us   (2.644 us .. 4.196 us)
        variance introduced by outliers: 56% (severely inflated)

        benchmarking exMJordy/m7^m8 mod m9
        time                 112.7 us   (110.8 us .. 114.9 us)
                             0.996 R²   (0.994 R² .. 0.998 R²)
        mean                 111.8 us   (110.4 us .. 114.0 us)
        std dev              5.649 us   (3.916 us .. 8.870 us)
        variance introduced by outliers: 52% (severely inflated)

        ------------------------------------------------------

        benchmarking exMFloris/m1^m2 mod m3
        time                 9.199 us   (9.087 us .. 9.333 us)
                             0.998 R²   (0.997 R² .. 0.999 R²)
        mean                 9.241 us   (9.139 us .. 9.383 us)
        std dev              419.1 ns   (323.0 ns .. 538.4 ns)
        variance introduced by outliers: 56% (severely inflated)

        benchmarking exMFloris/m4^m5 mod m6
        time                 42.41 us   (41.71 us .. 43.30 us)
                             0.997 R²   (0.996 R² .. 0.999 R²)
        mean                 42.78 us   (42.22 us .. 43.56 us)
        std dev              2.345 us   (1.837 us .. 3.199 us)
        variance introduced by outliers: 60% (severely inflated)

        benchmarking exMFloris/m5^m6 mod m7
        time                 60.24 us   (58.27 us .. 61.96 us)
                             0.994 R²   (0.991 R² .. 0.997 R²)
        mean                 57.59 us   (56.63 us .. 58.75 us)
        std dev              3.527 us   (2.909 us .. 4.580 us)
        variance introduced by outliers: 64% (severely inflated)

        benchmarking exMFloris/m6^m7 mod m8
        time                 62.81 us   (61.43 us .. 64.31 us)
                             0.997 R²   (0.996 R² .. 0.999 R²)
        mean                 62.57 us   (61.98 us .. 63.29 us)
        std dev              2.211 us   (1.861 us .. 2.664 us)
        variance introduced by outliers: 37% (moderately inflated)

        benchmarking exMFloris/m7^m8 mod m9
        time                 109.6 us   (107.7 us .. 111.7 us)
                             0.996 R²   (0.993 R² .. 0.998 R²)
        mean                 111.9 us   (110.0 us .. 114.8 us)
        std dev              7.492 us   (5.638 us .. 10.01 us)
        variance introduced by outliers: 66% (severely inflated)

        ------------------------------------------------------

        benchmarking expMOrg/m1^m2 mod m3
        time                 42.60 ns   (41.13 ns .. 44.55 ns)
                             0.993 R²   (0.988 R² .. 0.999 R²)
        mean                 41.52 ns   (40.95 ns .. 42.46 ns)
        std dev              2.412 ns   (1.773 ns .. 3.754 ns)
        variance introduced by outliers: 78% (severely inflated)

        benchmarking expMOrg/m4^m5 mod m6
        time                 3.722 us   (3.653 us .. 3.798 us)
                             0.995 R²   (0.993 R² .. 0.997 R²)
        mean                 3.866 us   (3.770 us .. 3.997 us)
        std dev              368.8 ns   (265.9 ns .. 586.1 ns)
        variance introduced by outliers: 86% (severely inflated)


        benchmarking expMOrg/m5^m6 mod m7
        time                 191.1 us   (184.5 us .. 198.1 us)
                             0.993 R²   (0.988 R² .. 0.997 R²)
        mean                 192.1 us   (188.1 us .. 198.3 us)
        std dev              16.60 us   (12.02 us .. 24.81 us)
        variance introduced by outliers: 75% (severely inflated)

        benchmarking expMOrg/m6^m7 mod m8
        time                 823.4 us   (809.5 us .. 845.4 us)
                             0.994 R²   (0.990 R² .. 0.997 R²)
        mean                 835.1 us   (820.9 us .. 854.1 us)
        std dev              54.84 us   (39.42 us .. 81.05 us)
        variance introduced by outliers: 54% (severely inflated)

        benchmarking expMOrg/m7^m8 mod m9
        <interactive>: warning: GetProcessTimes from kernel32 is linked instead of __imp_GetProcessTimes
        <interactive>: getMBlocks: VirtualAlloc MEM_COMMIT failed: The paging file is too small for this operation to complete.

    -}