import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a
-- Group FT_7

says :: Boy -> Boy -> Bool
says Matthew x
        | x == Carl                     = False
        | x == Matthew                  = False
        | otherwise                     = True

says Peter x
        | x == Matthew                  = True
        | x == Jack                     = True
        | otherwise                     = False

says Jack x = (not (says Matthew x)) && (not (says Peter x))

says Arnold x = (says Matthew x) `xor` (says Peter x)

says Carl x = (not (says Arnold x))

accusers :: Boy -> [Boy]
accusers target = filter (\b -> says b target) boys

guilty, honest :: [Boy]
guilty = [x | x <- boys, let h = accusers x, length h == 3]
honest = concatMap accusers guilty