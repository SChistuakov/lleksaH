f :: Integer -> [Integer] -> Integer
f a x | (head x) == a = 0
	| otherwise = (f a (tail x) + 1)