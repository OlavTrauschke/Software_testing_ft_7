{-The refactored code below is easier to modify for 'NRC-sudoku's than the original code,
because now no code has to change to add a constrnt, except for constrntsUnion,
to which a new constrnts should be appended. This modification was implemented in
Assignment52NRC.hs

We tested using the three consistent examples with a unique solution, example1,
example2 and example3. We had these examples solved with WinGHCi set to display
execution times, while no other programs while running (except for background
processes obviously, which we assume did not use significantly different
resources during the test).

Using the implementation from the lecture, we got the following execution times:
- example1: 0.03 secs
- example2: 0.02 secs
- example3: 0.09 secs
Using the refactored implementation below, we got the following execution times:
- example1: 0.05 secs
- example2: 0.03 secs
- example3: 0.34 secs

It seems like the refactored implementation is less efficiënt than the one from
the lecture, at least time-wise.-}

module Assignment52

where 

import Data.List
import System.Random

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]
type Position = (Row,Column)

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
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

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = Position -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> Position -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

freeAtPos :: Sudoku -> Position -> [Value]
freeAtPos s p = freeAtPos' s p constrntsUnion

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s p xs = let
    ys = filter (elem p) xs
  in foldl1 intersect (map ((values \\) . map s) ys)

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

consistent :: Sudoku -> Bool
consistent s = and [injective (map s c) | c <- constrnts]
  where constrnts = map (filter (\ x -> s x /= 0)) constrntsUnion

extend :: Sudoku -> (Position,Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x

type Constraint = (Position,[Value])
type Constrnt = [[Position]]

rowConstrnt =    [[(r,c) | c <- values] | r <- values]
columnConstrnt = [[(r,c) | r <- values] | c <- values]
blockConstrnt =  [[(r,c) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks]

constrntsUnion :: [[Position]]
constrntsUnion = rowConstrnt ++ columnConstrnt ++ blockConstrnt

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (p,vs) = 
   [(extend s (p,v),
     sortBy lengthValue $ 
         prune (p,v) constraints) | v <- vs ]

lengthValue :: ((a,b),[c]) -> ((a,b),[c]) -> Ordering
lengthValue (_,xs) (_,xs') = compare (length xs) (length xs')

prune :: (Position,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (p,v) ((p',zs):rest) 
  | elem p' influenced = (p',zs\\[v]) : prune (p,v) rest
  | otherwise = (p',zs) : prune (p,v) rest
  where influenced = foldr1 union (filter (elem p) constrntsUnion)

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [Position]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy lengthValue 
    [(p, freeAtPos s p) | 
                       p <- openPositions s ]

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

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