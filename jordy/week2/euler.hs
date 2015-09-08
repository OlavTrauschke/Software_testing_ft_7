problem1 :: Integer
problem1 = sum [x | x <- [0..999], mod x 3 == 0 || mod x 5 == 0]

problem2 :: Integer
