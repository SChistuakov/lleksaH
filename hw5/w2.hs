f :: (Eq a) => (a -> Bool) -> [a] -> Bool
f x y = all x y