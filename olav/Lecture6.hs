module Lecture6

where 

import Data.List
import System.Random
import System.CPUTime
import Test.QuickCheck

factors_naive :: Integer -> [Integer]
factors_naive n = factors' n 2 where 
  factors' 1 _ = []
  factors' n m 
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n = let 
    ps = takeWhile (\m -> m^2 <= n) primes
  in factors' n ps where 
     factors' 1 _  = []
     factors' n [] = [n]
     factors' n (p:ps) 
       | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
       | otherwise      =    factors' n ps

isPrime n = factors n == [n]
primes = 2 : filter isPrime [3..]

m1  = 2^2-1;    m2  = 2^3-1;     m3  = 2^5-1
m4  = 2^7-1;    m5  = 2^13-1;    m6  = 2^17-1 
m7  = 2^19-1;   m8  = 2^31-1;    m9  = 2^61-1
m10 = 2^89-1;   m11 = 2^107-1;   m12 = 2^127-1
m13 = 2^521-1;  m14 = 2^607-1;   m15 = 2^1279-1
m16 = 2^2203-1; m17 = 2^2281-1;  m18 = 2^3217-1
m19 = 2^4253-1; m20 = 2^4423-1;  m21 = 2^9689-1
m22 = 2^9941-1; m23 = 2^11213-1; m24 = 2^19937-1
m25 = 2^21701-1

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 

invM :: Integer -> Integer -> Integer
invM x n = let
    (u,v) = fct_gcd x n
    copr  = x*u + v*n == 1
    i     = if signum u == 1 then u else u + n  
  in 
     if copr then i else error "no inverse"

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
    then (1,0) 
    else 
      let 
          (q,r) = quotRem a b
          (s,t) = fct_gcd b r 
        in (t, s - q*t)

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

--To check this function is faster than expM we ran "checkExecutionTime 100 []". This test
--gave that exM was faster in 69 of 100 cases.
exM :: Integer -> Integer -> Integer -> Integer
exM x y n = let
    (exp,rem) = if y == 0 then (0,0) else decomp y
  in exM' x (exp,rem) n

exM' :: Integer -> (Integer,Integer) -> Integer -> Integer
exM' x (0,rem)   n = expM x rem n
exM' x (exp,rem) n = expM (exM' x (exp-1,rem) n) 2 n

--Test property to test exM with QuickCheck using expM as reference
prop_exMEqualsExpM :: Integer -> Integer -> Integer -> Bool
prop_exMEqualsExpM x y z 
  | z == 0 = True
  | otherwise = let w = normalizeParams x y z in (uncurry3 expM) w == (uncurry3 exM) w

normalizeParams :: Integer -> Integer -> Integer -> (Integer,Integer,Integer)
normalizeParams x y z
  | z <  0    = normalizeParams x y (-z)
  | y < 0     = (x,(-y),z)
  | otherwise = (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

--Speed comparison
exMFasterThanExpM :: Integer -> Integer -> Integer -> IO Bool
exMFasterThanExpM x y z
  | z == 0 = return True
  | otherwise = do
      startExM <- getCPUTime
      exMResult <- return (exM x y z)
      print exMResult --do something with the result to have it calculated
      endExM <- getCPUTime
      startExpM <- getCPUTime
      expMResult <- return (expM x y z)
      print expMResult --do something with the result to have it calculated
      endExpM <- getCPUTime
      return ((endExM - startExM) < (endExpM - startExpM))

--Compare execution times n times on random Integers
checkExecutionTime :: Int -> [Bool] -> IO String
checkExecutionTime 0 results = do
  testsRan <- (return.length) results
  faster <- (return.sum.(map fromEnum)) results
  return ("Faster in " ++ show faster ++ " of " ++ show testsRan ++ " cases.")
checkExecutionTime n results = do
  x <- getRandomInt (0,2000000)
  y <- getRandomInt (0,2000000)
  z <- getRandomInt (0,2000000)
  result <- exMFasterThanExpM x y z
  checkExecutionTime (n-1) (result:results)

--Get a random Int between -100 and 100
--Based strongly on getRandomInt from lecture 2,
--found at http://homepages.cwi.nl/~jve/courses/15/testing/lectures/Lecture2.html
getRandomInt :: (Integer,Integer) -> IO Integer
getRandomInt = getStdRandom.randomR

prime_test_F :: Integer -> IO Bool
prime_test_F n = do 
  a <- randomRIO (1, n-1) :: IO Integer
  return (exM a (n-1) n == 1)

prime_tests_F :: Int -> Integer -> IO Bool
prime_tests_F k n = do
  as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
  return (all (\ a -> exM a (n-1) n == 1) as)

decomp :: Integer -> (Integer,Integer)
decomp n = decomp' (0,n) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = let 
    (r,s) = decomp (n-1) 
    f = \ x -> takeWhile (/= 1) 
      (map (\ j -> exM x (2^j*s) n)  [0..r])
  in 
    do 
      a <- randomRIO (1, n-1) :: IO Integer
      if exM a (n-1) n /= 1 
        then return False 
        else 
          if exM a s n /= 1 && last (f a) /= (n-1) 
            then return False
            else primeMR (k-1) n

encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = m*k `mod` p

decodeDH :: Integer -> Integer -> Integer 
         -> Integer -> Integer -> Integer
decodeDH p k ga b c = let 
    gab' = exM ga ((p-1)-b) p 
  in 
    rem (c*gab') p

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let 
    p' = p-1
    e  = head [ x | x <- [k..], gcd x p' == 1 ]
  in 
    exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let 
    p' = p-1
    e  = head [ x | x <- [k..], gcd x p' == 1 ]
    d  = invM e p' 
  in 
    exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

rsa_public :: Integer -> Integer -> (Integer,Integer)
rsa_public p q = let 
    n   = p * q
    phi = (p-1)*(q-1)
    e   = head [ x | x <- [3..], gcd x phi == 1 ]
  in 
    (e,p*q)

rsa_private ::  Integer -> Integer 
                -> (Integer,Integer)
rsa_private p q = let 
    n = p * q
    phi = (p-1)*(q-1)
    e = head [ x | x <- [3..], gcd x phi == 1 ]
    d = invM e phi 
  in 
    (d,p*q)

rsa_encode :: (Integer,Integer) -> Integer -> Integer 
rsa_encode (e,n) =  \ m -> exM m e n

rsa_decode = rsa_encode

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsa_encode 

secret = m18
bound  = 131