module Assignment55

where

import Assignment51
import Data.List
import Lecture5 (emptyN,getRandomCnstr,randomize,filledPositions,eraseN)

{-Function are copies of functions of Lecture5, copied to have them reference to functions
from Assignment51 instead of the original ones from the lecture. Each function is a copy of
the function with the same name from the lecture, except when otherwise specified. We spent
about an hour implementing this rather small changes to the given code, because we tought
our solution did not work and the execution was stuck in an infinite loop, while it actually
just needs about a minute to generate a minimal NRC sudoku on the laptop we worked on.-}

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s <- genProblem r
          showNode s

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return 
                          (extendNode (s,cs\\xs) (head xs))

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False