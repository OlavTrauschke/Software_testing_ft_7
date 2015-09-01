pythagoreanTriplet :: Integer
pythagoreanTriplet = head [a*b*c | a <- [1..333], b <- [1..333], c <- [1..333], a^2+b^2==c^2, a+b+c==1000]