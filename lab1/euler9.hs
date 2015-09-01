problem9 = head [ a * b * c | (a, b, c) <- pythagoreanTriplets, a + b + c == 1000]
    where pythagoreanTriplets = [(a, b, c) | c <- [3..], b <- [2..(c-1)], a <- [1..(b-1)], a^2 + b^2 == c^2]