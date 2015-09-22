module Assignment5

where

import Rel
import Data.List
import Lecture4

trClos :: Ord a => Rel a -> Rel a
trClos = fp trStep
    where trStep r = sort $ nub $ r ++ (r @@ r)