module Assignment3 where
	
    import Lecture6

    composites :: [Integer]
    composites = filter (not.isPrime) [0..] 