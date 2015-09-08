import Data.Char

strip :: String -> String
strip s = filter (\c -> isAlphaNum c) s

ucase :: String -> String
ucase s = map (\c -> toUpper c) s

shiftBack :: Int -> String -> String
shiftBack n s = concat [(drop n s), (take n s)]

-- A/Z = 10/35, A = 65 ASCII
ibanMap :: Char -> String
ibanMap c = show ((ord (toUpper c)) - 55) 

iban :: String -> Bool
iban s = 
    let -- it go
        s' = shiftBack 4 (strip s)
        s'' = concatMap (\c -> if (isAlpha c) then (ibanMap c) else [c]) s'
        (i, _) = head (reads s'' :: [(Integer, String)]) -- safe, but not safe.
        rem = i `mod` 97
    in rem == 1

-- Validation
validIbans = [
    "QA58 DOHB 0000 1234 5678 90AB CDEF G",
    "BH67 BMAG 0000 1299 1234 56",
    "FR14 2004 1010 0505 0001 3M02 606",
    "NL39 RABO 0300 0652 64"
    ]

-- Generic tester, msgs accumulates error messages
tester :: Show a => (a -> Bool) -> [String] -> [a] -> IO ()
tester _ msgs []
    | length msgs == 0      = putStrLn "Passed all cases"
    | otherwise             = putStr (unlines msgs)
tester f msgs (c:cs)
    | (f c) == True         = tester f msgs cs
    | otherwise             = tester f (("Failed test case " ++ (show c)) : msgs) cs

testIbans = tester iban [] validIbans