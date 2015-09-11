import Data.List

problem1 :: Integer
problem1 = sum [x | x <- [0..999], mod x 3 == 0 || mod x 5 == 0]

problem2 :: Integer
problem2 = sum (filter (even) (takeWhile (< 4000000) fib))

-- fibonaci sequence generator from Rosetta Code
fib :: [Integer]
fib = 1 : 2 : zipWith (+) fib (tail fib)