  module Assignment2
  
  where 
  
  import Data.List
  import System.Random

  {-
   -
   - Time taken: 1 hours
   - 
   - *Assignment2> solveAndShow nrcExample
   - +---------+---------+---------+
   - | 4  7  8 | 3  9  2 | 6  1  5 |
   - |   +-----|--+   +--|-----+   |
   - | 6 |1  9 | 7| 5 |8 | 3  2| 4 |
   - | 2 |3  5 | 4| 1 |6 | 9  7| 8 |
   - +---------+---------+---------+
   - | 7 |2  6 | 8| 3 |5 | 1  4| 9 |
   - |   +-----|--+   +--|-----+   |
   - | 8  9  1 | 6  2  4 | 7  5  3 |
   - |   +-----|--+   +--|-----+   |
   - | 3 |5  4 | 9| 7 |1 | 2  8| 6 |
   - +---------+---------+---------+
   - | 5 |6  7 | 2| 8 |9 | 4  3| 1 |
   - | 9 |8  3 | 1| 4 |7 | 5  6| 2 |
   - |   +-----|--+   +--|-----+   |
   - | 1  4  2 | 5  6  3 | 8  9  7 |
   - +---------+---------+---------+
   - [()]
   - (0.09 secs, 13447104 bytes)
   - 
   - Extendability
   - The altered version of assignment2 is way easier to modify for new constraints. You only need to 
   - define a new constrnt, and add it to constrnts. The new program will take care of the rest.
   - While with the first version, you had to edit four locations.
   -
   - Efficiency
   - The new code also seems faster, by using ":set +s", haskell will time all functions. The differents
   - is not mind blowing, but on the NRC problem still 0.02 secs faster on avarage.
   - 
   -}

  type Row    = Int 
  type Column = Int 
  type Value  = Int
  type Grid   = [[Value]]

  type Position = (Row,Column)
  type Constrnt = [[Position]]
  
  positions, values :: [Int]
  positions = [1..9]
  values    = [1..9]

  rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
  columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
  blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
  nrcBlockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

  -- Allows users to add and remove constraints easily.
  constrnts = [rowConstrnt,columnConstrnt,blockConstrnt,nrcBlockConstrnt]
  
  blocks :: [[Int]]
  blocks = [[1..3],[4..6],[7..9]]

  nrcBlocks :: [[Int]]
  nrcBlocks = [[2..4],[6..8]]

  showVal :: Value -> String
  showVal 0 = " "
  showVal d = show d

  showRow :: Int -> [Value] -> IO()
  showRow n [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
      do  putStr "| "
          putStr (showVal a1) ; putChar ' '
          case n of
              0 -> putChar ' '
              1 -> putChar '|'
          putStr (showVal a2) ; putStr "  "
          putStr (showVal a3) ; putStr " | "
          putStr (showVal a4) ; 
          case n of
              0 -> putChar ' '
              1 -> putChar '|'
          putChar ' '; putStr (showVal a5) ; putChar ' '
          case n of
              0 -> putChar ' '
              1 -> putChar '|'
          putStr (showVal a6) ; putStr " | "
          putStr (showVal a7) ; putStr "  "
          putStr (showVal a8) ;
          case n of
              0 -> putChar ' '
              1 -> putChar '|'
          putChar ' '
          putStr (showVal a9) ; putStr " |\n"

  showGrid :: Grid -> IO()
  showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
      do putStrLn "+---------+---------+---------+"
         showRow 0 as
         putStrLn "|   +-----|--+   +--|-----+   |"
         showRow 1 bs; showRow 1 cs
         putStrLn "+---------+---------+---------+"
         showRow 1 ds
         putStrLn "|   +-----|--+   +--|-----+   |"
         showRow 0 es
         putStrLn "|   +-----|--+   +--|-----+   |"
         showRow 1 fs
         putStrLn "+---------+---------+---------+"
         showRow 1 gs; showRow 1 hs
         putStrLn "|   +-----|--+   +--|-----+   |"
         showRow 0 is
         putStrLn "+---------+---------+---------+"

  type Sudoku = (Row,Column) -> Value

  sud2grid :: Sudoku -> Grid
  sud2grid s = 
    [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 
  
  grid2sud :: Grid -> Sudoku
  grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> (Row,Column) -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

  showSudoku :: Sudoku -> IO()
  showSudoku = showGrid . sud2grid

  -- Altered by intersecting on all the constrnts
  freeAtPos :: Sudoku -> (Row,Column) -> [Value]
  freeAtPos s (r,c) = foldl intersect values (map (freeAtPos' s (r,c)) constrnts)

  -- foldl1 -> foldl, to account for empty lists (maybe a bit hacky)
  freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
  freeAtPos' s (r,c) xs = let 
     ys = filter (elem (r,c)) xs 
   in 
     foldl intersect values (map ((values \\) . map s) ys)

  injective :: Eq a => [a] -> Bool
  injective xs = nub xs == xs

  -- one generic injective function for all constrnts
  constrntInjective :: Sudoku -> Constrnt -> Bool
  constrntInjective s = all (injective.filter (/=0).map s)

  consistent :: Sudoku -> Bool
  consistent s = all (constrntInjective s) constrnts

  extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
  extend = update

  update :: Eq a => (a -> b) -> (a,b) -> a -> b 
  update f (y,z) x = if x == y then z else f x 

  type Constraint = (Row,Column,[Value])

  type Node = (Sudoku,[Constraint])
 
  showNode :: Node -> IO()
  showNode = showSudoku . fst

  solved  :: Node -> Bool
  solved = null . snd

  extendNode :: Node -> Constraint -> [Node]
  extendNode (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune (r,c,v) constraints) | v <- vs ]

  length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
  length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

  prune :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune _ [] = []
  prune (r,c,v) ((x,y,zs):rest)
    | any (any (elem (x,y)).filter (elem (r,c))) constrnts = (x,y,zs\\[v]) : prune (r,c,v) rest
    | otherwise = (x,y,zs) : prune (r,c,v) rest

  initNode :: Grid -> [Node]
  initNode gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints s)]

  openPositions :: Sudoku -> [(Row,Column)]
  openPositions s = [ (r,c) | r <- positions,  
                              c <- positions, 
                              s (r,c) == 0 ]

  constraints :: Sudoku -> [Constraint] 
  constraints s = sortBy length3rd 
      [(r,c, freeAtPos s (r,c)) | 
                         (r,c) <- openPositions s ]

  data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

  exmple1 = T 1 [T 2 [], T 3 []]
  exmple2 = T 0 [exmple1,exmple1,exmple1]

  grow :: (node -> [node]) -> node -> Tree node 
  grow step seed = T seed (map (grow step) (step seed))

  count :: Tree a -> Int 
  count (T _ ts) = 1 + sum (map count ts)

  search :: (node -> [node]) 
         -> (node -> Bool) -> [node] -> [node]
  search children goal [] = []
  search children goal (x:xs) 
    | goal x    = x : search children goal xs
    | otherwise = search children goal (children x ++ xs)

  solveNs :: [Node] -> [Node]
  solveNs = search succNode solved 
  
  succNode :: Node -> [Node]
  succNode (s,[]) = []
  succNode (s,p:ps) = extendNode (s,ps) p 

  solveAndShow :: Grid -> IO[()]
  solveAndShow gr = solveShowNs (initNode gr)
  
  solveShowNs :: [Node] -> IO[()]
  solveShowNs = sequence . fmap showNode . solveNs

  example1 :: Grid
  example1 = [[5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  example2 :: Grid
  example2 = [[0,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  example3 :: Grid
  example3 = [[1,0,0,0,3,0,5,0,4],
              [0,0,0,0,0,0,0,0,3],
              [0,0,2,0,0,5,0,9,8], 
              [0,0,9,0,0,0,0,3,0],
              [2,0,0,0,0,0,0,0,7],
              [8,0,3,0,9,1,0,6,0],
              [0,5,1,4,7,0,0,0,0],
              [0,0,0,3,0,0,0,0,0],
              [0,4,0,0,0,9,7,0,0]]

  example4 :: Grid
  example4 = [[1,2,3,4,5,6,7,8,9],
              [2,0,0,0,0,0,0,0,0],
              [3,0,0,0,0,0,0,0,0],
              [4,0,0,0,0,0,0,0,0],
              [5,0,0,0,0,0,0,0,0],
              [6,0,0,0,0,0,0,0,0],
              [7,0,0,0,0,0,0,0,0],
              [8,0,0,0,0,0,0,0,0],
              [9,0,0,0,0,0,0,0,0]]

  example5 :: Grid
  example5 = [[1,0,0,0,0,0,0,0,0],
              [0,2,0,0,0,0,0,0,0],
              [0,0,3,0,0,0,0,0,0],
              [0,0,0,4,0,0,0,0,0],
              [0,0,0,0,5,0,0,0,0],
              [0,0,0,0,0,6,0,0,0],
              [0,0,0,0,0,0,7,0,0],
              [0,0,0,0,0,0,0,8,0],
              [0,0,0,0,0,0,0,0,9]]

  nrcExample :: Grid
  nrcExample = [[0,0,0,3,0,0,0,0,0],
                [0,0,0,7,0,0,3,0,0],
                [2,0,0,0,0,0,0,0,8],
                [0,0,6,0,0,5,0,0,0],
                [0,9,1,6,0,0,0,0,0],
                [3,0,0,0,7,1,2,0,0],
                [0,0,0,0,0,0,0,3,1],
                [0,8,0,0,4,0,0,0,0],
                [0,0,0,0,0,0,0,0,0]]