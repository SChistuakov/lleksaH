f :: [Int] -> [Int]
f x = rev x []

rev :: [Int] -> [Int] -> [Int]
rev [] l = l
rev (x:xs) l = rev xs (x:l)