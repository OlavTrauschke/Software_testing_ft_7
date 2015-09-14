import Data.Char
import Test.QuickCheck

-- A thin monadic skin layer
getList :: IO [Char]
getList = fmap take5 getContents
 
-- The actual worker
take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])
