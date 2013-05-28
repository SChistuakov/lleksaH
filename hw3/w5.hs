data Tree a = Nil
            | Branch (Tree a) (Tree a)

f :: (Tree a) -> Int
f Nil = 0
f (Branch l r) = 1 + min (f l) (f r)