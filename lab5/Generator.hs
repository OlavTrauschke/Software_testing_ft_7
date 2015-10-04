module Generator where

  import Solver

  import Data.List
  import System.Random

  emptyN :: Config -> Node
  emptyN cfg = (\ _ -> 0,(constraints cfg) (\ _ -> 0))

  getRandomInt :: Int -> IO Int
  getRandomInt n = getStdRandom (randomR (0,n))

  getRandomItem :: [a] -> IO [a]
  getRandomItem [] = return []
  getRandomItem xs = do n <- getRandomInt maxi
                        return [xs !! n]
                     where maxi = length xs - 1

  randomize :: Eq a => [a] -> IO [a]
  randomize xs = do y <- getRandomItem xs 
                    if null y 
                      then return []
                      else do ys <- randomize (xs\\y)
                              return (head y:ys)

  sameLen :: Constraint -> Constraint -> Bool
  sameLen (_,_,xs) (_,_,ys) = length xs == length ys

  getRandomCnstr :: [Constraint] -> IO [Constraint]
  getRandomCnstr cs = getRandomItem (f cs) 
    where f [] = []
          f (x:xs) = takeWhile (sameLen x) (x:xs)

  rsuccNode :: Config -> Node -> IO [Node]
  rsuccNode cfg (s,cs) = do xs <- getRandomCnstr cs
                            if null xs 
                              then return []
                              else return 
                                (extendNode cfg (s,cs\\xs) (head xs))

  rsolveNs :: Config -> [Node] -> IO [Node]
  rsolveNs cfg ns = rsearch (rsuccNode cfg) solved (return ns)

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

  uniqueSol :: Config -> Node -> Bool
  uniqueSol cfg node = singleton (solveNs cfg [node]) where 
    singleton [] = False
    singleton [x] = True
    singleton (x:y:zs) = False

  eraseS :: Sudoku -> (Row,Column) -> Sudoku
  eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                       | otherwise      = s (x,y)

  eraseN :: Config -> Node -> (Row,Column) -> Node
  eraseN cfg n (r,c) = (s, constraints cfg s) 
    where s = eraseS (fst n) (r,c) 

  minimalize :: Config -> Node -> [(Row,Column)] -> Node
  minimalize _ n [] = n
  minimalize cfg n ((r,c):rcs) | uniqueSol cfg n' = minimalize cfg n' rcs
                               | otherwise        = minimalize cfg n  rcs
    where n' = eraseN cfg n (r,c) 

  -- Minimal property, a node should be uniquely solvable by itself, consistent and erasing any position
  -- should no longer yield a unique solution.
  prop_minimal :: Config -> Node -> Bool
  prop_minimal cfg node =
    let s = fst node in
      (consistent cfg s) && 
      (uniqueSol cfg node) &&
      (not $ any (uniqueSol cfg) (map (eraseN cfg node) (filledPositions s)))

  -- Shortcut, usage: test def 100 (prop_minimal def)
  test :: Config -> Int -> (Node -> Bool) -> IO ()
  test cfg = testR cfg 0

  -- Modified version of lecture 2.
  testR :: Config -> Int -> Int -> (Node -> Bool) -> IO ()
  testR cfg k n f = if k == n then putStr (show n ++ " tests passed\n")
                  else do
                    node <- (genRandomProblem cfg)
                    if f node then
                      do putStr "pass on:\n"
                         showNode cfg node
                         putStr (show (k+1) ++ "/" ++ show n ++ "\n")
                         testR cfg (k+1) n f
                    else 
                      do putStr "failed test on:\n"
                         showNode cfg node
                         error ("Failed on test " ++ show (k+1) ++ "/" ++ show n)

  -- Generators.
  genRandomSudoku :: Config -> IO Node
  genRandomSudoku cfg = do [r] <- rsolveNs cfg [emptyN cfg]
                           return r
  genRandomProblem :: Config -> IO Node
  genRandomProblem cfg = do [r] <- rsolveNs cfg [emptyN cfg]
                            genProblem cfg r

  genProblem :: Config -> Node -> IO Node
  genProblem cfg n = do ys <- randomize xs
                        return (minimalize cfg n ys)
     where xs = filledPositions (fst n)

  genEmptyBlocksProblem :: Int -> IO Node
  genEmptyBlocksProblem n = do try <- genEmptyBlocks n 
                               if uniqueSol def try then return try
                               else genEmptyBlocksProblem n

  genEmptyBlocks :: Int -> IO Node
  genEmptyBlocks n = 
    let rm :: Node -> Int -> [[Position]] -> Node
        rm s 0 _        = s
        rm s n' (b:bs)  = rm (clearBlock s b) (n'-1) bs
    in 
    do  [r] <- rsolveNs def [emptyN def]
        rbc <- randomize bc
        r' <- return (rm r n rbc)
        genProblem def r'

  clearBlock :: Node -> [Position] -> Node
  clearBlock s [] = s
  clearBlock s (b:bs) = clearBlock (eraseN def s b) bs

  -- Compare average hints for NRC and standard
  bench :: Config -> Int -> Int -> Int -> IO ()
  bench _ 0 sum done = 
    let avg = sum `div` done in 
    putStrLn("Done, n = " ++ (show done) ++ ", tot = " ++ (show sum) ++ ", avg = " ++ (show avg) ++ " hints")
  bench cfg n sum done = do
    rnd <- genRandomProblem cfg
    showNode cfg rnd
    bench cfg (n-1) (sum + length (filledPositions (fst rnd))) (done+1)