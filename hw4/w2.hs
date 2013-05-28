data Tree a = Nil
            | Branch a (Tree a) (Tree a)

f :: (a -> a -> a) -> a -> (Tree a) -> a	
f _ x Nil = x
f c x (Branch v l r) = f c (c v (f c x l)) r 