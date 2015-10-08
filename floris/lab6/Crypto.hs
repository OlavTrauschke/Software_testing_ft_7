module Crypto where
    
import Data.List
import System.Random
import Control.Monad
import Lecture6

composites :: [Integer]
composites = composites' [0..]

composites' :: [Integer] -> [Integer]
composites' = filter (not . isPrime)

smallestFool :: (Integer -> IO Bool) -> [Integer] -> IO Integer
smallestFool f (n:ns) = do 
  m <- f n
  if m then return n else smallestFool f ns

--minMatch :: Int -> (Integer -> IO Bool) -> [Integer] -> IO Integer
--minMatch n f xs = foldMmapM (\_ -> smallestFool f xs) [1..n]

carmichael :: [Integer]
carmichael = 
  [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       isPrime (6*k+1),
       isPrime (12*k+1),
       isPrime (18*k+1) ]

-- Carmichael blalal coprime, randomIO. carmichael >>> composites
mersenne :: [Integer]
mersenne = [ p | p <- map (\ i -> 2^i-1) primes, (w $ prime_tests_F 3 p), (w $ primeMR 3 p) ]