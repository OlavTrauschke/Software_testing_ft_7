module Assignment1 where

    import Lecture6

    exMJ :: Integer -> Integer -> Integer -> Integer
    exMJ b e m = exMJ' b e m 1

    exMJ' :: Integer -> Integer -> Integer -> Integer -> Integer
    exMJ' _ _ 1 r = r
    exMJ' b 0 m r = r
    exMJ' b e m r | odd e = exMJ' b (e-1) m (b * r `rem` m)
                  | otherwise = exMJ' (b * b `rem` m) (e `div` 2) m r


    exMF :: Integer -> Integer -> Integer -> Integer
    exMF _ _ 1 = 0
    exMF _ 0 _ = 1
    exMF b e m
       | odd e       = exMF b (e-1) m * b `mod` m
       | otherwise   = exMF (b * b `mod` m) (e `div` 2) m