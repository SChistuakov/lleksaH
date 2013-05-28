f :: [Int] -> Maybe Int
f [x] = Nothing
f (x:y:[]) = Nothing
f (x:y:z:[]) = if (x<y) && (y>z)
			then Just y
			else Nothing
f (x:y:z:xs) = if (x<y) && (y>z)
			then Just y
			else f (y:z:xs)