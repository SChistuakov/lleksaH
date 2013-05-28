f :: Int -> [[Int]]
f n = helper n n
	where 
		helper 0 _  = [[]]
		helper n1 n2 = [1..n2] >>= (\x -> map (x:) (helper (n1 - x) (min x (n1 - x))))