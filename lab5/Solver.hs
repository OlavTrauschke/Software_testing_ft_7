module Solver where
  
  import Data.List

  -- Instead of building two nearly identical solver implementations, we have
  -- one. Because Monads are not well understood at this point, we're dragging
  -- a config structure along with a lot of calls, sorry for that.

  type Row    = Int 
  type Column = Int 
  type Value  = Int
  type Grid   = [[Value]]

  positions, values :: [Int]
  positions = [1..9]
  values    = [1..9]

  type Position = (Row,Column)
  type Constrnt = [[Position]]

  type Sudoku = (Row,Column) -> Value

  type Constraint = (Row,Column,[Value])
  type Node = (Sudoku,[Constraint])
  
  -- Configuration consists of a print function and set of constraints.
  data Config = Config { consts :: [Constrnt], showGrid :: Grid -> IO() }

  blocks, nrcBlocks :: [[Int]]
  blocks = [[1..3],[4..6],[7..9]]
  nrcBlocks = [[2..4],[6..8]] -- special NRC blocks

  rc, cc, bc, nc :: Constrnt
  rc = [[(r,c)| c <- values ] | r <- values ]
  cc = [[(r,c)| r <- values ] | c <- values ]
  bc = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
  nc = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

  -- Two presets
  nrc = Config { consts = [rc,cc,bc,nc], showGrid = showGridNrc }
  def = Config { consts = [rc,cc,bc], showGrid = showGridDef }

  sud2grid :: Sudoku -> Grid
  sud2grid s = 
    [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 
  
  grid2sud :: Grid -> Sudoku
  grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> (Row,Column) -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1) 

  -- Intersecting on all the constraints with possible values
  freeAtPos :: Config -> Sudoku -> (Row,Column) -> [Value]
  freeAtPos cfg s (r,c) = foldl' intersect values (map (freeAtPos' s (r,c)) (consts cfg))

  freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
  freeAtPos' s (r,c) xs = let 
     ys = filter (elem (r,c)) xs 
   in 
     foldl' intersect values (map ((values \\) . map s) ys) -- foldl1 -> foldl, to account for [] (maybe a bit hacky)

  injective :: Eq a => [a] -> Bool
  injective xs = nub xs == xs

  -- One injective function for all constrnts
  constrntInjective :: Sudoku -> Constrnt -> Bool
  constrntInjective s = all (injective.filter (/=0).map s)

  consistent :: Config -> Sudoku -> Bool
  consistent cfg s = all (constrntInjective s) (consts cfg)

  extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
  extend = update

  update :: Eq a => (a -> b) -> (a,b) -> a -> b 
  update f (y,z) x = if x == y then z else f x 

  solved  :: Node -> Bool
  solved = null . snd

  extendNode :: Config -> Node -> Constraint -> [Node]
  extendNode cfg (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune cfg (r,c,v) constraints) | v <- vs ]

  length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
  length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

  prune :: Config -> (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune _ _ [] = []
  prune cfg (r,c,v) ((x,y,zs):rest)
    | any (any (elem (x,y)).filter (elem (r,c))) (consts cfg) = (x,y,zs\\[v]) : prune cfg (r,c,v) rest
    | otherwise = (x,y,zs) : prune cfg (r,c,v) rest

  initNode :: Config -> Grid -> [Node]
  initNode cfg gr = let s = grid2sud gr in 
                if (not . consistent cfg) s then [] 
                else [(s, constraints cfg s)]

  openPositions :: Sudoku -> [(Row,Column)]
  openPositions s = [ (r,c) | r <- positions,  
                              c <- positions, 
                              s (r,c) == 0 ]

  filledPositions :: Sudoku -> [(Row,Column)]
  filledPositions s = [ (r,c) | r <- positions,  
                                c <- positions, s (r,c) /= 0 ]

  constraints :: Config -> Sudoku -> [Constraint] 
  constraints cfg s = sortBy length3rd 
      [(r,c, freeAtPos cfg s (r,c)) | (r,c) <- openPositions s ]

  search :: (node -> [node]) 
         -> (node -> Bool) -> [node] -> [node]
  search children goal [] = []
  search children goal (x:xs) 
    | goal x    = x : search children goal xs
    | otherwise = search children goal (children x ++ xs)

  solveNs :: Config -> [Node] -> [Node]
  solveNs cfg = search (succNode cfg) solved 
  
  succNode :: Config -> Node -> [Node]
  succNode _ (s,[]) = []
  succNode cfg (s,p:ps) = extendNode cfg (s,ps) p 

  solveAndShow :: Config -> Grid -> IO[()]
  solveAndShow cfg gr = solveShowNs cfg (initNode cfg gr)
  
  solveShowNs :: Config -> [Node] -> IO[()]
  solveShowNs cfg = sequence . fmap (showNode cfg) . (solveNs cfg)

  -- Printing.
  showNode :: Config -> Node -> IO()
  showNode cfg = showSudoku cfg . fst

  showSudoku :: Config -> Sudoku -> IO()
  showSudoku cfg = showGrid cfg . sud2grid

  showVal :: Value -> String
  showVal 0 = " "
  showVal d = show d

  showGridDef :: Grid -> IO()
  showGridDef [as,bs,cs,ds,es,fs,gs,hs,is] =
    let
      showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
         do  putChar '|'         ; putChar ' '
             putStr (showVal a1) ; putChar ' '
             putStr (showVal a2) ; putChar ' '
             putStr (showVal a3) ; putChar ' '
             putChar '|'         ; putChar ' '
             putStr (showVal a4) ; putChar ' '
             putStr (showVal a5) ; putChar ' '
             putStr (showVal a6) ; putChar ' '
             putChar '|'         ; putChar ' '
             putStr (showVal a7) ; putChar ' '
             putStr (showVal a8) ; putChar ' '
             putStr (showVal a9) ; putChar ' '
             putChar '|'         ; putChar '\n'
    in
   do putStrLn ("+-------+-------+-------+")
      showRow as; showRow bs; showRow cs
      putStrLn ("+-------+-------+-------+")
      showRow ds; showRow es; showRow fs
      putStrLn ("+-------+-------+-------+")
      showRow gs; showRow hs; showRow is
      putStrLn ("+-------+-------+-------+")

  showGridNrc :: Grid -> IO()
  showGridNrc [as,bs,cs,ds,es,fs,gs,hs,is] =
      let 
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
      in
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

  -- Examples.
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

  nrcExample2 :: Grid
  nrcExample2 = [[0,0,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0,3],
                 [0,0,0,0,0,8,0,0,0],
                 [0,0,0,0,0,9,0,4,0],
                 [0,6,0,0,0,0,0,0,0],
                 [0,0,5,0,0,0,0,0,0],
                 [8,0,4,3,0,0,0,0,0],
                 [0,0,0,0,0,0,0,2,0],
                 [0,0,0,0,0,0,0,0,0]]

  hardmode1 :: Grid
  hardmode1 = [[7,5,0,0,0,0,0,4,0],
               [0,9,0,0,6,0,0,0,8],
               [0,0,0,0,0,3,2,5,0],
               [0,0,8,1,0,0,0,0,7],
               [0,7,0,0,5,0,0,2,0],
               [9,0,0,0,0,6,1,0,0],
               [0,6,7,3,0,0,0,0,0],
               [2,0,0,0,9,0,0,6,0],
               [0,3,0,0,0,0,0,7,1]]

  hardmode2 :: Grid
  hardmode2 = [[8,0,0,0,0,0,0,0,0],
               [0,0,3,6,0,0,0,0,0],
               [0,7,0,0,9,0,2,0,0],
               [0,5,0,0,0,7,0,0,0],
               [0,0,0,0,4,5,7,0,0],
               [0,0,0,1,0,0,0,3,0],
               [0,0,1,0,0,0,0,6,8],
               [0,0,8,5,0,0,0,1,0],
               [0,9,0,0,0,0,4,0,0]]