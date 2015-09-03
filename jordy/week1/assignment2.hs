import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

permu = [([a,b],[c,d,e]) | a <- boys, b <- (delete a boys), c <- delete b (delete a boys), d <- delete c (delete b (delete a boys)), e <- delete d (delete c (delete b (delete a boys)))]