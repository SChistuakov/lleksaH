import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

f :: (Integral a) => [a] -> Int
f x = fromJust (elemIndex (maximum l) l) + 1
	where l = zipWith (+) x (tail x)