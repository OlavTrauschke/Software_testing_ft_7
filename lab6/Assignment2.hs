module Assignment2 where

    import Lecture6
    import Assignment1
    import System.Random
    import System.CPUTime
    import Criterion.Main -- cabal install criterion

    {-
     - While testing the performance of our different implementations of the modular exponentiation function, some interesting results arrised.
     - Our are much alike, as they are both from the same idea. But the implementation of floris does seem slightly faster in all usecases described here.
     - Finaly the original implementation is in the smallest cases quicker than any of our implementations, this is expected, as our implementations
     - have much more to calculate, and squaring and dividing is not that much of a problem on smaller numbers. When numbers get bigger though, is
     - when this implementation breaks down. While our implementation does not even take twice as much time calculating m7^m8 mod m9 over m6^m7 mod m8,
     - the original implementation is four times slower. The next calculation is even so expensive, that haskell crashes before it can return a value,
     - due to being out of memory. And m6 and m7 are not even close to a prime of 1024 bits, which is (at least) needed for a safe private key.
     - (https://www.google.nl/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=rsa+minimum+key+size)
     -}

    -- Implementation of the criterion performance test, for all 5 implementations.
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
        bgroup "exMOlav"   [ bench "m1^m2 mod m3"  $ nf (exMO m1 m2) m3,
                             bench "m4^m5 mod m6"  $ nf (exMO m4 m5) m6,
                             bench "m5^m6 mod m7"  $ nf (exMO m5 m6) m7,
                             bench "m6^m7 mod m8"  $ nf (exMO m6 m7) m8,
                             bench "m7^m8 mod m9"  $ nf (exMO m7 m8) m9
                           ],
        bgroup "Rosetta"   [ bench "m1^m2 mod m3"  $ nf (powmR' m1 m2) m3,
                             bench "m4^m5 mod m6"  $ nf (powmR' m4 m5) m6,
                             bench "m5^m6 mod m7"  $ nf (powmR' m5 m6) m7,
                             bench "m6^m7 mod m8"  $ nf (powmR' m6 m7) m8,
                             bench "m7^m8 mod m9"  $ nf (powmR' m7 m8) m9
                           ],
        bgroup "expMOrg"   [ bench "m1^m2 mod m3"  $ nf (expM m1 m2) m3,
                             bench "m4^m5 mod m6"  $ nf (expM m4 m5) m6,
                             bench "m5^m6 mod m7"  $ nf (expM m5 m6) m7,
                             bench "m6^m7 mod m8"  $ nf (expM m6 m7) m8,
                             bench "m7^m8 mod m9"  $ nf (expM m7 m8) m9 -- Crashes the benchmark!
                           ]
        ]

    -- Olav's implementation of a custom performance test.
    {-To check exM is faster than expM we ran "check 100". This test gave that exM "ran 100.0%
    faster on average", meaning exM ran in less than 0.05% of the time expM ran in on average.-}

    check :: Int -> IO String
    check n = checkExecutionTime n (1000000,1000000,1000000) (9999999,9999999,9999999)
                                 (uncurry3 powmR') (uncurry3 expM)

    --Compare execution times of two functions n times on arbitrary input between specified
    --limits
    checkExecutionTime :: (Random a,Show a) => Int -> a -> a -> (a -> b) -> (a -> b)
                          -> IO String
    checkExecutionTime = checkExecutionTime' []

    checkExecutionTime' :: (Random a,Show a) => [Int] -> Int -> a -> a -> (a -> b) -> (a -> b)
                           -> IO String
    checkExecutionTime' results 0 _ _ _ _ = do
      testsRan <- (return.length) results
      averageDifference <- return ((fromIntegral.sum) results / fromIntegral testsRan)
      return ("The first function ran " ++ show averageDifference ++ " % faster on average.")
    checkExecutionTime' results n min max f g = do
      x <- randomRIO (min,max)
      result <- timeDifference f g x
      checkExecutionTime' (result:results) (n-1) min max f g

    --Determine the percentage of the execution time of the second provided function the first
    --provided function takes less
    timeDifference :: (a -> b) -> (a -> b) -> a -> IO Int
    timeDifference f g x = do
      startF <- getCPUTime
      resultF <- return $! (f x)
      endF <- getCPUTime
      resultG <- return $! (g x)
      endG <- getCPUTime
      timeF <- return (endF - startF)
      timeG <- return (endG - endF)
      difference <- return (timeG - timeF)
      return (round (fromIntegral difference / fromIntegral timeG * 100))

    instance (Random a,Random b,Random c) => Random (a,b,c) where
      randomRIO ((minX,minY,minZ),(maxX,maxY,maxZ)) = do
        x <- randomRIO (minX,maxX)
        y <- randomRIO (minY,maxY)
        z <- randomRIO (minZ,maxZ)
        return (x,y,z)

    uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
    uncurry3 f (x,y,z) = f x y z

    {-
        ------------------->> BENCHMARKS <<-------------------
        - JORDY ----------------------------------------------

        benchmarking exMJordy/m1^m2 mod m3
        time                 11.20 μs   (11.13 μs .. 11.28 μs)
                             1.000 R²   (0.999 R² .. 1.000 R²)
        mean                 11.24 μs   (11.15 μs .. 11.36 μs)
        std dev              342.1 ns   (276.4 ns .. 441.3 ns)
        variance introduced by outliers: 36% (moderately inflated)

        benchmarking exMJordy/m4^m5 mod m6
        time                 50.46 μs   (49.94 μs .. 51.17 μs)
                             0.998 R²   (0.998 R² .. 0.999 R²)
        mean                 50.53 μs   (49.92 μs .. 51.19 μs)
        std dev              2.247 μs   (1.918 μs .. 2.643 μs)
        variance introduced by outliers: 49% (moderately inflated)

        benchmarking exMJordy/m5^m6 mod m7
        time                 64.90 μs   (64.15 μs .. 65.56 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 65.09 μs   (64.59 μs .. 65.71 μs)
        std dev              1.946 μs   (1.629 μs .. 2.521 μs)
        variance introduced by outliers: 29% (moderately inflated)

        benchmarking exMJordy/m6^m7 mod m8
        time                 74.91 μs   (73.70 μs .. 76.86 μs)
                             0.993 R²   (0.980 R² .. 0.999 R²)
        mean                 75.16 μs   (73.79 μs .. 78.54 μs)
        std dev              6.721 μs   (3.380 μs .. 13.63 μs)
        variance introduced by outliers: 79% (severely inflated)

        benchmarking exMJordy/m7^m8 mod m9
        time                 125.5 μs   (124.1 μs .. 126.9 μs)
                             0.999 R²   (0.998 R² .. 0.999 R²)
        mean                 125.7 μs   (124.4 μs .. 127.1 μs)
        std dev              4.557 μs   (3.948 μs .. 5.501 μs)
        variance introduced by outliers: 35% (moderately inflated)

        - FLORIS ---------------------------------------------

        benchmarking exMFloris/m1^m2 mod m3
        time                 11.52 μs   (11.25 μs .. 11.89 μs)
                             0.997 R²   (0.995 R² .. 0.999 R²)
        mean                 11.39 μs   (11.28 μs .. 11.54 μs)
        std dev              434.0 ns   (343.6 ns .. 637.6 ns)
        variance introduced by outliers: 46% (moderately inflated)

        benchmarking exMFloris/m4^m5 mod m6
        time                 52.99 μs   (52.65 μs .. 53.49 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 53.22 μs   (52.81 μs .. 53.78 μs)
        std dev              1.655 μs   (1.415 μs .. 2.074 μs)
        variance introduced by outliers: 32% (moderately inflated)

        benchmarking exMFloris/m5^m6 mod m7
        time                 69.17 μs   (68.72 μs .. 69.80 μs)
                             0.999 R²   (0.998 R² .. 0.999 R²)
        mean                 69.99 μs   (69.29 μs .. 70.81 μs)
        std dev              2.618 μs   (2.090 μs .. 3.373 μs)
        variance introduced by outliers: 39% (moderately inflated)

        benchmarking exMFloris/m6^m7 mod m8
        time                 79.97 μs   (78.97 μs .. 81.39 μs)
                             0.996 R²   (0.992 R² .. 0.999 R²)
        mean                 81.37 μs   (80.14 μs .. 83.75 μs)
        std dev              5.425 μs   (3.598 μs .. 8.600 μs)
        variance introduced by outliers: 67% (severely inflated)

        benchmarking exMFloris/m7^m8 mod m9
        time                 137.3 μs   (133.0 μs .. 144.1 μs)
                             0.991 R²   (0.980 R² .. 0.999 R²)
        mean                 133.0 μs   (131.4 μs .. 136.4 μs)
        std dev              7.737 μs   (3.978 μs .. 14.63 μs)
        variance introduced by outliers: 58% (severely inflated)

        - OLAV -----------------------------------------------

        benchmarking exMOlav/m1^m2 mod m3
        time                 12.63 μs   (12.54 μs .. 12.73 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 12.73 μs   (12.61 μs .. 12.88 μs)
        std dev              447.5 ns   (377.8 ns .. 531.3 ns)
        variance introduced by outliers: 42% (moderately inflated)

        benchmarking exMOlav/m4^m5 mod m6
        time                 56.41 μs   (55.88 μs .. 56.94 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 56.80 μs   (56.33 μs .. 57.42 μs)
        std dev              1.764 μs   (1.379 μs .. 2.619 μs)
        variance introduced by outliers: 31% (moderately inflated)

        benchmarking exMOlav/m5^m6 mod m7
        time                 73.81 μs   (73.30 μs .. 74.48 μs)
                             0.999 R²   (0.999 R² .. 0.999 R²)
        mean                 74.92 μs   (74.18 μs .. 75.71 μs)
        std dev              2.506 μs   (2.224 μs .. 2.939 μs)
        variance introduced by outliers: 34% (moderately inflated)

        benchmarking exMOlav/m6^m7 mod m8
        time                 83.16 μs   (82.51 μs .. 83.81 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 83.45 μs   (82.73 μs .. 84.17 μs)
        std dev              2.469 μs   (2.058 μs .. 3.415 μs)
        variance introduced by outliers: 28% (moderately inflated)

        benchmarking exMOlav/m7^m8 mod m9
        time                 141.4 μs   (139.4 μs .. 143.9 μs)
                             0.998 R²   (0.997 R² .. 0.999 R²)
        mean                 142.5 μs   (141.2 μs .. 144.5 μs)
        std dev              5.000 μs   (4.053 μs .. 6.324 μs)
        variance introduced by outliers: 33% (moderately inflated)

        - ROSETTA --------------------------------------------

        benchmarking Rosetta/m1^m2 mod m3
        time                 7.624 μs   (7.454 μs .. 7.882 μs)
                             0.995 R²   (0.989 R² .. 0.999 R²)
        mean                 7.523 μs   (7.440 μs .. 7.667 μs)
        std dev              371.6 ns   (223.7 ns .. 660.0 ns)
        variance introduced by outliers: 61% (severely inflated)

        benchmarking Rosetta/m4^m5 mod m6
        time                 29.55 μs   (29.28 μs .. 29.83 μs)
                             0.999 R²   (0.999 R² .. 0.999 R²)
        mean                 29.54 μs   (29.27 μs .. 29.88 μs)
        std dev              1.043 μs   (811.2 ns .. 1.315 μs)
        variance introduced by outliers: 39% (moderately inflated)

        benchmarking Rosetta/m5^m6 mod m7
        time                 38.30 μs   (37.89 μs .. 38.70 μs)
                             0.999 R²   (0.999 R² .. 0.999 R²)
        mean                 38.35 μs   (37.98 μs .. 38.76 μs)
        std dev              1.311 μs   (1.056 μs .. 1.690 μs)
        variance introduced by outliers: 37% (moderately inflated)

        benchmarking Rosetta/m6^m7 mod m8
        time                 42.38 μs   (41.80 μs .. 43.07 μs)
                             0.999 R²   (0.997 R² .. 0.999 R²)
        mean                 42.47 μs   (42.00 μs .. 43.07 μs)
        std dev              1.707 μs   (1.266 μs .. 2.323 μs)
        variance introduced by outliers: 45% (moderately inflated)

        benchmarking Rosetta/m7^m8 mod m9
        time                 73.79 μs   (72.99 μs .. 74.40 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 73.57 μs   (72.91 μs .. 74.37 μs)
        std dev              2.417 μs   (2.006 μs .. 2.981 μs)
        variance introduced by outliers: 33% (moderately inflated)

        - ORIGINAL -------------------------------------------

        benchmarking expMOrg/m1^m2 mod m3
        time                 78.41 ns   (77.65 ns .. 79.13 ns)
                             0.999 R²   (0.998 R² .. 0.999 R²)
        mean                 77.95 ns   (77.16 ns .. 79.15 ns)
        std dev              3.225 ns   (2.452 ns .. 4.904 ns)
        variance introduced by outliers: 63% (severely inflated)

        benchmarking expMOrg/m4^m5 mod m6
        time                 2.023 μs   (1.999 μs .. 2.059 μs)
                             0.999 R²   (0.997 R² .. 1.000 R²)
        mean                 2.019 μs   (2.003 μs .. 2.041 μs)
        std dev              63.31 ns   (46.54 ns .. 105.8 ns)
        variance introduced by outliers: 42% (moderately inflated)

        benchmarking expMOrg/m5^m6 mod m7
        time                 55.67 μs   (55.23 μs .. 56.13 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 55.94 μs   (55.49 μs .. 56.37 μs)
        std dev              1.539 μs   (1.288 μs .. 1.796 μs)
        variance introduced by outliers: 27% (moderately inflated)

        benchmarking expMOrg/m6^m7 mod m8
        time                 289.3 μs   (286.7 μs .. 292.0 μs)
                             0.999 R²   (0.999 R² .. 1.000 R²)
        mean                 291.4 μs   (288.9 μs .. 294.5 μs)
        std dev              9.399 μs   (7.791 μs .. 12.37 μs)
        variance introduced by outliers: 27% (moderately inflated)

        benchmarking expMOrg/m6^m7 mod m8
        FAILED, after a couple of hours and having hogged 20gb of memory was still not done.

    -}