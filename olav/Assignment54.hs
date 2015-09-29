module Assignment54 where

import Lecture5 hiding (main,genProblem)

{-As proven by Emrakul in his reply from May 21 '14, edited on the same day, as
found on September 29 '15 at http://puzzling.stackexchange.com/questions/309/
what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have, sudoku's with four
empty blocks are possible, but it is impossible to have a sudoku with five empty blocks.

Below is a generator for sudoku's with three empty blocks. Until now I spent about three
hours on this exercise.-}

--Copy of main from the lecture, using my own genProblem instead of the original one
main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s <- genProblem r
          showNode s

--Based on genProblem from the lecture
genProblem :: Node -> IO Node
genProblem n = do xs <- randomize (completeBlocks)
                  m <- removeBlocks (n,xs) 3
                  ys <- randomize (filledPositions (fst m))
                  return (minimalize m ys)

completeBlocks :: [[(Row,Column)]]
completeBlocks = completeBlocks' 9

completeBlocks' :: Int -> [[(Row,Column)]]
completeBlocks' 0 = []
completeBlocks' n = [(x,y) | x <- (blocks !! div (n-1) 3), y <- (blocks !! mod n 3)]
                     : completeBlocks' (n-1)

removeBlocks :: (Node,[[(Row,Column)]]) -> Int -> Node
removeBlocks (node,_)      0 = node
removeBlocks (node,blocks) n = removeBlocks (removeBlock node blocks) (n-1)

removeBlock :: Node -> [[(Row,Column)]] -> (Node,[[(Row,Column)]])