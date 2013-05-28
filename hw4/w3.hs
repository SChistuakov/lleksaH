f :: (Integral a) => [a] -> a
f xs =  length (filter even xs)

f0 :: (Integral a) => [a] -> a
f0 xs = sum (map (\ x -> mod x 2) xs)

f1 :: (Integral a) => [a] -> Int
f1 xs = foldr (\x acc -> if (odd x) then acc + 1 else acc) 0 xs
