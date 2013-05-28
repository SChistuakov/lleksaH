f :: (Eq a) => [a] -> Bool

f xs = helper [] xs
	where 
		helper_ [] = True
		helper(ys) (z:zs) = if elem z ys
					then False
					else helper (z:ys) zs