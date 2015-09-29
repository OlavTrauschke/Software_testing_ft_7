{-We ran check 100 to test the generated sudoku's are minimal, which returned true. We spent
about two hours on this exercise, mostly because we tried to use quickCheck at first, which
we found was harder than we tought because the function we had to test returns an IO Node
instead of a Node or a Gen Node-}

module Assignment53

where

import Control.Monad
import Lecture5

--own variant of quickCheck to easily handle IO monad
check :: Int -> IO Bool
check 0 = return True
check n = liftM2 (&&) (liftM prop_minimal getRandomProblem) (check (n-1))

prop_minimal :: Sudoku -> Bool
prop_minimal s = consistent s && (uniqueSol.head.initNode) g && (not.minimizable) s
  where g = sud2grid s

minimizable :: Sudoku -> Bool
minimizable s = any (\ (r,c,v) -> let s' = extend s ((r,c),0)
                                  in uniqueSol (s',constraints s')) (sud2rcvs s)

sud2rcvs :: Sudoku -> [(Row,Column,Value)]
sud2rcvs s = filter (\ (_,_,v) -> v /= 0) [(r,c,s (r,c)) | r <- positions, c <- positions]

getRandomProblem :: IO Sudoku
getRandomProblem = do
  [r] <- rsolveNs [emptyN]
  n <- genProblem r
  return (fst n)