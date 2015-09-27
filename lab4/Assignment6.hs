module Assignment6

where

import Rel
import Data.List
import Lecture4

trClos :: Ord a => Rel a -> Rel a
trClos r = fp (\r -> sort $ union r (r @@ r)) r