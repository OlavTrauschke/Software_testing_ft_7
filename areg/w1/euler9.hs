problem9' = head [ a * b * c | (a, b, c) <- pythagoreanTriplets, a + b + c == 1000]
    where pythagoreanTriplets = [(a, b, c) | c <- [3..], b <- [2..(c-1)], a <- [1..(b-1)], a^2 + b^2 == c^2]


problem9 :: [(Int,Int,Int)]
problem9 =
	let
		cs = [334..997]
		bs = [2..333]
		as = [1..332]
		triplets = [(a,b,c) | c<-cs, b<-bs, a<-as, a<b, b<c, a^2+b^2==c^2]
	in
		triplets
