import Control.Exception

data Boy = Matthew | Peter | Jack | Arnold | Carl
	deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Maybe Bool
says Matthew	Carl	= Just False
says Matthew	Matthew	= Just False
says Peter	Peter	= Just False
says Peter	Arnold	= Just False
says Peter	Carl	= Just False
says Jack	x	= says Arnold x &&& says Carl x &&& notMaybe (says Matthew x) &&& notMaybe (says Peter x)
says Arnold	x	= (says Matthew x &&& notMaybe (says Peter x)) ||| (says Peter x &&& notMaybe (says Matthew x))
says Carl	x	= notMaybe (says Arnold x)
says _		_	= Nothing

(&&&) :: Maybe Bool -> Maybe Bool -> Maybe Bool
x &&& y
	| x == Just False = Just False
	| y == Just False = Just False
	| otherwise = Just True

(|||) :: Maybe Bool -> Maybe Bool -> Maybe Bool
x ||| y
	| x == Just True = Just True
	| y == Just True = Just True
	| otherwise = Just False

notMaybe :: Maybe Bool -> Maybe Bool
notMaybe Nothing = Nothing
notMaybe x = x

accusers :: Boy -> [Boy]
accusers x = filter (\y -> (says y x) == Just True) boys

guilty, honest :: [Boy]
