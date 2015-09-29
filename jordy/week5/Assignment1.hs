module Assignment1 where

import Data.List
import System.Random
import Lecture5

{-
 -
 - Time taken: 3,5 hours
 - 
 - *Assignment1> solveAndShowNrc nrcExample
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
 - (0.11 secs, 24259912 bytes)
 - 
 -}


-- Redifined blocks' to return a list of tuples, where each tuple is a list of rows, columns
blocks' :: [([Row],[Column])]
blocks' = map (\(a,b) -> ([a..a+2],[b..b+2])) coords
    where coords = [(1,1),(1,4),(1,7),(4,1),(4,4),(7,4),(7,1),(7,4),(7,7),(2,2),(2,6),(6,2),(6,6)]

-- Returns a list of blocks a coordinate belongs to using the new blocks'
bl' :: (Row, Column) -> [([Row],[Column])]
bl' (r,c) = filter (\(rs,cs) -> elem r rs && elem c cs) blocks'

-- Returns a list of values for each of the subgrids a coordinate is part of
subGrid' :: Sudoku -> (Row,Column) -> [[Value]]
subGrid' s (r,c) = map ((\vs -> [s v | v <- vs]).(\(rs,cs) -> [(r',c') | r' <- rs, c' <- cs])) (bl' (r,c))

-- Returns the values still available for a specific coordinate
freeInSubgrid' :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid' s (r,c) = foldr (intersect.freeInSeq) values (subGrid' s (r,c))

-- Checks all subgrids for a coordinate for injectivity
subgridInjective' :: Sudoku -> (Row,Column) -> Bool
subgridInjective' s (r,c) = all injective (map (filter (/= 0)) (subGrid' s (r,c)))

-- Checks sudoku for consistance, added the check for the NRC blocks.
consistent' :: Sudoku -> Bool
consistent' s = and $
              [ rowInjective s r |  r <- positions ]
               ++
              [ colInjective s c |  c <- positions ]
               ++
              [ subgridInjective' s (r,c) | 
                   r <- [1,4,7], c <- [1,4,7]]
               ++
              [ subgridInjective' s (r,c) | 
                   r <- [2,6], c <- [2,6]]

-- From Lecture5.hs, but altered the call to freeInSubgrid -> freeInSubgrid'
freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
freeAtPos' s (r,c) = 
                freeInRow s r
    `intersect` freeInColumn s c
    `intersect` freeInSubgrid' s (r,c)

-- From Lecture5.hs, but altered the call to freeAtPos -> freeAtPos'
constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c)) | (r,c) <- openPositions s ]

-- Returns if two coordinates share any blocks
sameblock' :: (Row,Column) -> (Row,Column) -> Bool
sameblock' p1 p2 = (not.null) $ intersect (bl' p1)  (bl' p2)

-- From Lecture5.hs, but altered the call to prune -> prune'
extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints) (r,c,vs) = 
    [(extend s ((r,c),v),
        sortBy length3rd $ 
            prune' (r,c,v) constraints) | v <- vs ]

-- From Lecture5.hs, but altered the call to sameblock -> sameblock'
prune' :: (Row,Column,Value) 
    -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | sameblock' (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune' (r,c,v) rest
    | otherwise = (x,y,zs) : prune' (r,c,v) rest 

-- From Lecture5.hs, but altered the call to consistent -> consistent', call to constraints -> constraints'
initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
               if (not . consistent') s then [] 
               else [(s, constraints' s)]

-- From Lecture5.hs, but altered the call to succNode -> succNode'
solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

-- From Lecture5.hs, but altered the call to extendNode -> extendNode'
succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p 

-- From Lecture5.hs, but altered the call to solveShowNs -> solveShowNs'
solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNs' (initNode' gr)

-- From Lecture5.hs, but altered the call to solveNs -> solveNs'
solveShowNs' :: [Node] -> IO[()]
solveShowNs' = sequence . fmap showNode' . solveNs'

showRow' :: Int -> [Value] -> IO()
showRow' n [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
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

showGrid' :: Grid -> IO()
showGrid' [as,bs,cs,ds,es,fs,gs,hs,is] =
    do putStrLn "+---------+---------+---------+"
       showRow' 0 as
       putStrLn "|   +-----|--+   +--|-----+   |"
       showRow' 1 bs; showRow' 1 cs
       putStrLn "+---------+---------+---------+"
       showRow' 1 ds
       putStrLn "|   +-----|--+   +--|-----+   |"
       showRow' 0 es
       putStrLn "|   +-----|--+   +--|-----+   |"
       showRow' 1 fs
       putStrLn "+---------+---------+---------+"
       showRow' 1 gs; showRow' 1 hs
       putStrLn "|   +-----|--+   +--|-----+   |"
       showRow' 0 is
       putStrLn "+---------+---------+---------+"

showNode' :: Node -> IO()
showNode' = showSudoku' . fst

showSudoku' :: Sudoku -> IO()
showSudoku' = showGrid' . sud2grid

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