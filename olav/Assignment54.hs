module Assignment54 where

import Data.Maybe
import Lecture5 hiding (main,genProblem)

{-As proven by Emrakul in his reply from May 21 '14, edited on the same day, as
found on September 29 '15 at http://puzzling.stackexchange.com/questions/309/
what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have, sudoku's with four
empty blocks are possible, but it is impossible to have a sudoku with five empty blocks.

Below is a generator for sudoku's with three empty blocks. We spent about four hours on this
exercise. This generator was built so that just changing the number 3 at line 31 is enough
to get sudoku's with other numbers of empty blocks, as long as this number is no longer than
four (otherwise this is not possible)-}

--Based on main from the lecture, using my own genProblem instead of the original one
--Calls itsself again if and only if it is not possible to clear three blocks in the
--randomly generated 
main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          maybeS <- genProblem r
          if (isJust maybeS) then do
            showNode r
            s <- return (fromJust maybeS)
            showNode s
          else main

--Based on genProblem from the lecture
genProblem :: Node -> IO (Maybe Node)
genProblem n = do xs <- randomize (completeBlocks numberOfBlocks)
                  m <- return (removeBlocks n xs 4)
                  if (snd m) then do
                    p <- return (fst m)
                    ys <- randomize (filledPositions (fst p))
                    return (Just (minimalize p ys))
                  else
                    return Nothing

completeBlocks :: Int -> [[(Row,Column)]]
completeBlocks 0 = []
completeBlocks n = [(x,y) | x <- (blocks !! div (n-1) 3), y <- (blocks !! mod n 3)]
                     : completeBlocks (n-1)

numberOfBlocks :: Int
numberOfBlocks = length blocks ^ 2

removeBlocks :: Node  -> [[(Row,Column)]] -> Int -> (Node,Bool)
removeBlocks node _ 0 = (node,True)
removeBlocks node [] _ = (node,False)
removeBlocks node (b:blocks) n
  | uniqueSol nodeWithoutB = let result = removeBlocks nodeWithoutB blocks (n-1)
      in if (snd result) then result else (removeBlocks node blocks n)
  | otherwise = removeBlocks node blocks n
  where nodeWithoutB = eraseBlock node b

eraseBlock :: Node -> [(Row,Column)] -> Node
eraseBlock node [] = node
eraseBlock node (x:xs) = eraseBlock (eraseN node x) xs