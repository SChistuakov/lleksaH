data Tree a = Nil
	  | Branch a (Tree a) (Tree a)

f :: (a-> Bool) -> (Tree a) -> Bool
f a Nil = False
searchTree a (Branch v l r) = if a v then True
				     else searchTree a l || searchTree a r