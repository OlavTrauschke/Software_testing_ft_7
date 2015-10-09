module Assignment4 where

    import Lecture6
    import Assignment3
    import Control.Monad

    smallestFooler :: (Integer -> IO Bool) -> [Integer] -> IO Integer
    smallestFooler f (n:ns) = do 
                                    p <- f n
                                    if p then return n else smallestFooler f ns

    findSmallestFooler :: Int -> (Integer -> IO Bool) -> [Integer] -> IO Integer
    findSmallestFooler n f ns = liftM minimum (mapM (\_ -> smallestFooler f ns) [1..n])