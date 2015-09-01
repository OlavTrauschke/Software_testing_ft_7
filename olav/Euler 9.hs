pythagoreanTriplet :: Integer
pythagoreanTriplet = head [a*b*c | c <- [333..500], b <- [1..(c-1)], a <- [1..(b-1)], a^2+b^2==c^2, a+b+c==1000]

--Running this results in 31875000