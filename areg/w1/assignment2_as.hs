-- Areg Shahbazian, Lab 01, Ex2


xor :: Bool -> Bool -> Bool
xor a b		|	a==True && b==False	= True
		|	a==False && b==True	= True
		|	otherwise		= False

		

data Boy = Matthew | Peter | Jack | Arnold | Carl
	deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
--says Jack b	|	b==Peter	=	False
--		|	b==Matthew	=	False

--says Arnold b	|	b==Matthew	=	True
--		|	b==Peter	=	True

--says Carl b	|	b==Arnold	=	False

---------------------------------------------------------

says Matthew b	|	b==Carl		=	False
		|	b==Matthew	=	False
		|	otherwise	=	True

says Peter b	|	b==Peter	=	False
		|	b==Arnold	=	False
		|	b==Carl		=	False
		|	otherwise	=	True

says Jack b	=	(not (says Matthew b)) && (not (says Peter b))
says Arnold b	=	(says Matthew b) `xor` (says Peter b)
says Carl b	= 	not (says Arnold b)


--accusers :: Boy -> [Boy]

--guilty, honest :: [Boy]


-- Euler problem 9

prob9 = take 1 [(a,b,c) | a <- [1..500], b <- [1..500], c <- [333..], a<b, b<c, a^2 + b^2 == c^2, a+b+c==1000]


triplets = take 1000 [(a,b,c) | a <- [1..334], b <- [1..334], c <- [333..1000] ]
































