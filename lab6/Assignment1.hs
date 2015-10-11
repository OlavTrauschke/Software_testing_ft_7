module Assignment1 where

    import Lecture6
    import Test.QuickCheck

    -- Floris's implementation inspired from https://en.wikipedia.org/wiki/Exponentiation_by_squaring#Basic_method
    exMF :: Integer -> Integer -> Integer -> Integer
    exMF _ _ 1 = 0
    exMF _ 0 _ = 1
    exMF b e m
       | odd e       = exMF b (e-1) m * b `mod` m
       | otherwise   = exMF (b * b `mod` m) (e `div` 2) m

    -- Jordy's implementation is a altered version of Floris's implementation
    exMJ :: Integer -> Integer -> Integer -> Integer
    exMJ b e m = exMJ' b e m 1

    exMJ' :: Integer -> Integer -> Integer -> Integer -> Integer
    exMJ' _ _ 1 r = 0
    exMJ' b 0 m r = r
    exMJ' b e m r | odd e = exMJ' b (e-1) m (b * r `rem` m)
                  | otherwise = exMJ' (b * b `rem` m) (e `div` 2) m r

    -- Olav's implementation based on a previous implementation of Jordy
    exMO :: Integer -> Integer -> Integer -> Integer
    exMO _ _ 1 = 0
    exMO b 0 m = 1
    exMO b e m
      | odd e     = multM b (exMO b (e-1) m) m
      | otherwise = exMO (multM b b m) (e `div` 2) m

    -- quickCheck property. Takes two modular exponentiation functions, and checks if the output matches.
    prop_expM :: (Integer -> Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer -> Integer) -> Positive Integer -> NonNegative Integer -> Positive Integer -> Bool
    prop_expM f1 f2 (Positive b) (NonNegative e) (Positive m) = f1 b e m == f2 b e m

    exCheck :: IO ()
    exCheck = do
        putStrLn "Checking Floris' implementation"
        quickCheck (prop_expM expM exMF)
        putStrLn "Checking Olav's implementation"
        quickCheck (prop_expM expM exMO)
        putStrLn "Checking Jordy's implementation"
        quickCheck (prop_expM expM exMJ)
        putStrLn "Checking Rosetta Code implementation"
        quickCheck (prop_expM expM powmR')

    -- Code from rosetta code altered to fix mod 1
    powmR :: Integer -> Integer -> Integer -> Integer -> Integer
    powmR _ _ 1 _ = 0
    powmR b 0 m r = r
    powmR b e m r | odd e = powmR (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
    powmR b e m r = powmR (b * b `mod` m) (e `div` 2) m r

    powmR' b e m = powmR b e m 1

    {-
     - *Assignment1> exCheck 
     - Checking Floris' implementation
     - +++ OK, passed 100 tests.
     - Checking Olav's implementation
     - +++ OK, passed 100 tests.
     - Checking Jordy's implementation
     - +++ OK, passed 100 tests.
     - Checking Rosetta Code implementation
     - +++ OK, passed 100 tests.
     -
     -}