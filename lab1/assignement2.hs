data Boy = Matthew | Peter | Jack | Arnold | Carl
	deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool

accusers :: Boy -> [Boy]

guilty, honest :: [Boy]