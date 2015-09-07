module IBAN where

import Data.Char

iban :: String -> Bool
iban n = hasAllowedChars n && calculateMod (read ((convertLetters.moveFirstFourChars.stripSpaces) n) :: Integer) == 1

-- Check for allowed chars (0-9A-Z(only uppercase!))
hasAllowedChars :: String -> Bool
hasAllowedChars = all (\n -> isDigit n || isUpper n || isSpace n)

-- Strip all spaces from the string
stripSpaces :: String -> String
stripSpaces [] = []
stripSpaces (x:xs) | x == ' '  = stripSpaces xs
                   | otherwise = x:(stripSpaces xs)

-- Move the first four chars to the back
moveFirstFourChars :: String -> String
moveFirstFourChars (a:b:c:d:es) = es ++ (a:b:c:[d])
moveFirstFourChars _ = error "String needs to be at least 4 chars long"

-- Convert Letters to their number representative (A=>10 ... Z=>35)
convertLetters :: String -> String
convertLetters [] = []
convertLetters (x:xs) | 65 <= (ord x) && (ord x) <= 90 = (show ((ord x) - 55)) ++ (convertLetters xs)
                      | otherwise = x:(convertLetters xs)

-- Calculate the mod 97
calculateMod :: Integer -> Integer
calculateMod n = mod n 97