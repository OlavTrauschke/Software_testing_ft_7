import Data.Char

-- 30 mins
iban :: String -> Bool
iban s = 
    let strip s         = filter (\c -> isAlphaNum c) s
        shiftBack n s   = concat [(drop n s), (take n s)]
        charMap c       = show ((ord (toUpper c)) - 55) -- A/Z = 10/35, A = 65 ASCII
        letterize s     = concatMap (\c -> if (isAlpha c) then (charMap c) else [c]) s
        toInt s         = if length s > 0 then read s :: Integer else 0
    in (toInt $ letterize $ shiftBack 4 (strip s)) `mod` 97 == 1

-- Validation
ibanChecks = [
        ("QA58 DOHB 0000 1234 5678 90AB CDEF G", True),
        ("BH67 BMAG 0000 1299 1234 56", True),
        ("FR14 2004 1010 0505 0001 3M02 606", True),
        ("NL39 RABO 0300 0652 64", True),
        ("NL39 RABO 0300 0642 64", False),
        ("&$908@#()*€___  64", False),
        ("", False)
    ]

-- Generic tester, msgs accumulates error messages
tester :: Show a => (a -> Bool) -> [String] -> [(a, Bool)] -> IO ()
tester _ msgs []
    | length msgs == 0      = putStrLn "Passed all cases"
    | otherwise             = putStr (unlines msgs)
tester f msgs ((c, e):cs)
    | (f c) == e            = tester f msgs cs
    | otherwise             = tester f (("Failed test case " ++ (show c) ++ ", should be " ++ (show e)) : msgs) cs

testIbans = tester iban [] ibanChecks